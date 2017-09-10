{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Solga.Router
  ( -- * Serving APIs
    serve, serveThrow
  -- * Abbreviation
  , Abbreviated(..)
  -- * Error handling
  , SolgaError
  , badRequest
  , notFound
  -- * Router implementation
  , FromSegment(..)
  , Router(..)
  , Responder
  , tryRouteNext
  , tryRouteNextIO
  ) where

import           Control.Applicative
import           Control.Exception.Safe
import           Control.Monad
import           Control.Monad.Trans.Resource
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text as Text
import           Data.Text.Encoding
import           GHC.Generics
import           GHC.TypeLits
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai
import qualified Network.HTTP.Types as HTTP

import           Solga.Core

---------------------------------------------------

-- | The right hand side of `Application`. `Request` is already known.
type Responder = (Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived

-- | Routers are the basic typeclass of Solga: their types describe
-- what type of requests they accept, and their values describe how to handle them.
--
-- You can use `Generic` to get free instance of `Router` for any data type with one constructor
-- and `Router`s as fields. The fields will be considered alternatives, as if you wrote `:<|>` between them.
class Router r where
  -- | Given a request, if the router supports the given request
  -- return a function that constructs a response with a concrete router.
  tryRoute :: Wai.Request -> Maybe (r -> Responder)
  default tryRoute :: (Generic r, Router (Rep r ())) => Wai.Request -> Maybe (r -> Responder)
  tryRoute = tryRouteNext (from :: r -> Rep r ())

-- | Try to route using a type @r@ by providing a function to turn it into a `Router` @r'@.
-- Useful for passing routing on to the next step.
tryRouteNext :: Router r' => (r -> r') -> Wai.Request -> Maybe (r -> Responder)
tryRouteNext f req = (. f) <$> tryRoute req

-- | Like `tryRouteNext` but in `IO`.
tryRouteNextIO :: Router r' => (r -> IO r') -> Wai.Request -> Maybe (r -> Responder)
tryRouteNextIO f req = do
  nextRouter <- tryRoute req
  Just $ \router cont -> do
    next <- f router
    nextRouter next cont

-- | Serve a `Router` with Solga, returning `SolgaError`s as HTTP responses and other errors as HTTP 500.
serve :: Router r => r -> Wai.Application
serve router req cont =
  serveThrow router req cont
    `catchAny` \someEx ->
      let
        ( status, body ) = case fromException someEx of
          Just SolgaError { errorStatus, errorMessage } -> ( errorStatus, Builder.byteString $ encodeUtf8 errorMessage )
          Nothing -> ( HTTP.internalServerError500, "Internal Server Error" )
      in cont $ Wai.responseBuilder status [] body

-- | Serve a `Router` with Solga, throwing `SolgaError`s.
serveThrow :: Router r => r -> Wai.Application
serveThrow router req cont = case tryRoute req of
  Nothing -> throwIO $ notFound ""
  Just r -> r router cont

instance (a ~ Wai.Application) => Router (Raw a) where
  tryRoute req = Just $ \(Raw app) -> app req

instance (a ~ Wai.Response) => Router (RawResponse a) where
  tryRoute _ = Just $ \(RawResponse response) cont -> cont response

instance Router next => Router (End next) where
  tryRoute req = case Wai.pathInfo req of
    [] -> tryRouteNext endNext req
    _ -> Nothing

instance (KnownSymbol seg, Router next) => Router (Seg seg next) where
  tryRoute req = case Wai.pathInfo req of
    s : segs | Text.unpack s == symbolVal (Proxy :: Proxy seg) ->
      tryRouteNext segNext req { Wai.pathInfo = segs }
    _ -> Nothing

instance (Router left, Router right) => Router (left :<|> right) where
  tryRoute req = tryRouteNext altLeft req <|> tryRouteNext altRight req

instance (KnownSymbol seg, Router next, Router (OneOfSegs segs next)) => Router (OneOfSegs (seg ': segs) next) where
  tryRoute = tryRouteNext $ \(OneOfSegs next) -> (Seg next :: Seg seg next) :<|> (OneOfSegs next :: OneOfSegs segs next)

instance Router next => Router (OneOfSegs '[] next) where
  tryRoute _ = Nothing

-- | The class of types that can be parsed from a path segment.
class FromSegment a where
  fromSegment :: Text.Text -> Maybe a

instance FromSegment Text.Text where
  fromSegment = Just

instance (FromSegment a, Router next) => Router (Capture a next) where
  tryRoute req = case Wai.pathInfo req of
    seg : segs -> do
      capture <- fromSegment seg
      tryRouteNext (\c -> captureNext c capture) req { Wai.pathInfo = segs }
    _ -> Nothing

instance (KnownSymbol method, Router next) => Router (Method method next) where
  tryRoute req = do
    guard (Char8.unpack (Wai.requestMethod req) == symbolVal (Proxy :: Proxy method))
    tryRouteNext methodNext req

instance Aeson.ToJSON a => Router (JSON a) where
  tryRoute _ = Just $ \json cont ->
    cont $ Wai.responseBuilder HTTP.status200 headers $ Aeson.fromEncoding $ Aeson.toEncoding $ jsonResponse json
      where headers = [ ( HTTP.hContentType, "application/json" ) ]

instance Router next => Router (ExtraHeaders next) where
  tryRoute req = do
    nextRouter <- tryRoute req
    return $ \(ExtraHeaders headers next) cont -> do
      let addHeaders oldHeaders = Map.assocs (Map.fromList headers `Map.union` Map.fromList oldHeaders)
      nextRouter next $ \response ->
        cont $ Wai.mapResponseHeaders addHeaders response

instance Router next => Router (NoCache next) where
  tryRoute = tryRouteNext (ExtraHeaders [cacheControlDisableCaching] . noCacheNext)
    where
      cacheControlDisableCaching = ("Cache-Control", "no-store, no-cache, must-revalidate, max-age=0")

instance (Aeson.FromJSON a, Router next) => Router (ReqBodyJSON a next) where
  tryRoute req = tryRouteNextIO getNext req
    where
      getNext rbj = do
        reqBody <- Wai.requestBody req
        case Aeson.eitherDecodeStrict reqBody of
          Left err -> throwIO $ badRequest $ "Could not decode JSON request: " <> Text.pack (show err)
          Right val -> return (reqBodyJSONNext rbj val)

instance Router next => Router (WithIO next) where
  tryRoute = tryRouteNextIO withIONext

instance (Router next) => Router (ReqBodyMultipart a next) where
  tryRoute req = flip fmap (tryRoute req) $ \nextRouter rmp cont ->
    runResourceT $ withInternalState $ \s -> do
      (params, fileInfos0) <- Wai.parseRequestBody (Wai.tempFileBackEnd s) req
      let fileInfos = do
            (parName, Wai.FileInfo{..}) <- fileInfos0
            return
              ( parName
              , MultiPartFileInfo
                  { mpfiName = fileName
                  , mpfiContentType = fileContentType
                  , mpfiContent = fileContent
                  }
              )
      let multiPart :: MultiPartData = (params, fileInfos)
      case reqMultiPartParse rmp multiPart of
        Left err -> throwIO $ badRequest $ "Could not decode form request: " <> Text.pack err
        Right val -> nextRouter (reqMultiPartNext rmp val) cont

-- | Most `Router`s are really just newtypes. By using `brief`, you can
--   construct trees of `Router`s by providing only their inner types, much
--   like Servant.
class Abbreviated a where
  type Brief a :: *
  type instance Brief a = a
  brief :: Brief a -> a
  default brief :: Brief a ~ a => Brief a -> a
  brief = id

instance Abbreviated (Raw a) where
  type Brief (Raw a) = a
  brief = Raw

instance Abbreviated (RawResponse a) where
  type Brief (RawResponse a) = a
  brief = RawResponse

instance Abbreviated next => Abbreviated (End next) where
  type Brief (End next) = Brief next
  brief = End . brief

instance Abbreviated next => Abbreviated (Seg seg next) where
  type Brief (Seg seg next) = Brief next
  brief = Seg . brief

instance (Abbreviated left, Abbreviated right) => Abbreviated (left :<|> right) where
  type Brief (left :<|> right) = Brief left :<|> Brief right
  brief (l :<|> r) = brief l :<|> brief r

instance Abbreviated next => Abbreviated (OneOfSegs segs next) where
  type Brief (OneOfSegs segs next) = Brief next
  brief = OneOfSegs . brief

instance Abbreviated next => Abbreviated (Capture a next) where
  type Brief (Capture a next) = a -> Brief next
  brief = Capture . fmap brief

instance Abbreviated next => Abbreviated (Method method next) where
  type Brief (Method method next) = Brief next
  brief = Method . brief

instance Abbreviated (JSON a) where
  type Brief (JSON a) = a
  brief = JSON

instance Abbreviated (ExtraHeaders next)

instance Abbreviated next => Abbreviated (NoCache next) where
  type Brief (NoCache next) = Brief next
  brief = NoCache . brief

instance Abbreviated next => Abbreviated (ReqBodyJSON a next) where
  type Brief (ReqBodyJSON a next) = a -> Brief next
  brief = ReqBodyJSON . fmap brief

instance Abbreviated next => Abbreviated (WithIO next) where
  type Brief (WithIO next) = IO (Brief next)
  brief = WithIO . fmap brief

instance Abbreviated (ReqBodyMultipart a next)

-- Generic routers

deriving instance Router r => Router (K1 i r p)
deriving instance Router (f p) => Router (M1 i c f p)

instance (Router (left p), Router (right p)) => Router ((left :*: right) p) where
  tryRoute req = routeLeft <|> routeRight
    where
      routeLeft = tryRouteNext (\(left :*: _) -> left) req
      routeRight = tryRouteNext (\(_ :*: right) -> right) req

-- Error handling

-- | A `Router`-related exception with a corresponding HTTP error code.
data SolgaError = SolgaError
  { errorStatus :: HTTP.Status
  , errorMessage :: Text.Text
  } deriving (Eq, Ord, Show)

instance Exception SolgaError

-- | Create a @400 Bad Request@ error with a given message.
badRequest :: Text.Text -> SolgaError
badRequest msg = SolgaError
  { errorStatus = HTTP.badRequest400
  , errorMessage = msg
  }

-- | Create a @404 Not Found@ error with a given message.
notFound :: Text.Text -> SolgaError
notFound msg = SolgaError
  { errorStatus = HTTP.notFound404
  , errorMessage = msg
  }

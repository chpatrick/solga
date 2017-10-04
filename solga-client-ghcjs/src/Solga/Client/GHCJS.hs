{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}
module Solga.Client.GHCJS
  ( Client(..)
  , SomeRequestData(..)
  , choose
  , RawRequest(..)
  , ToSegment(..)
  , WithData(..)
  , GetResponse(..)
  , Request(..)
  , Response(..)
  , Header
  , XHRError(..)
  ) where

import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits (symbolVal, KnownSymbol, Symbol)
import Data.Monoid ((<>))
import qualified Data.DList as DList
import Data.DList (DList)
import Data.String (fromString)
import qualified GHCJS.DOM.XMLHttpRequest as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Enums as DOM
import Data.Foldable (for_)
import Control.Monad.IO.Class (liftIO)
import Control.Exception.Safe (onException, bracket)
import Control.Concurrent.MVar (takeMVar, tryPutMVar, MVar, newEmptyMVar)
import qualified GHCJS.DOM.EventM as DOM.Event
import qualified GHCJS.DOM.XMLHttpRequestEventTarget as DOM.Event
import qualified GHCJS.DOM.XMLHttpRequest as DOM.XMLHttpRequest hiding (send)
import Control.Monad.Trans.Class (lift)
import Control.Monad (void)

import Solga.Core hiding (Header)

#if defined(ghcjs_HOST_OS)
import qualified JavaScript.JSValJSON as Json
import Data.JSString (JSString)
import qualified Data.JSString as T
import qualified GHCJS.DOM.JSFFI.Generated.XMLHttpRequest as DOM.XMLHttpRequest
type Text = JSString
#else
import qualified Data.Aeson as Json
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BSL
import qualified Language.Javascript.JSaddle as JSaddle
import qualified Network.HTTP.Types.URI as Http
import qualified Data.Binary.Builder as BB
import qualified JSDOM.Generated.XMLHttpRequest as DOM.XMLHttpRequest
#endif

type Header = (Text, Text)

data Request = forall body. (DOM.IsXMLHttpRequestBody body) => Request
  { reqMethod :: Text
  , reqSegments :: DList Text
  , reqQuery :: DList (Text, Maybe Text)
  , reqUser :: Maybe Text
  , reqPassword :: Maybe Text
  , reqHeaders :: [Header]
  , reqBody :: Maybe body
  , reqXHR :: DOM.XMLHttpRequest
  -- ^ will be used to send the request, useful if you want to set
  -- events on it (e.g. onprogress)
  }

data SomeRequestData out a = forall in_. (Client in_) => SomeRequestData (Proxy in_) (RequestData in_ a)

class Client r where
  type RequestData r :: * -> *
  type RequestData r = SomeRequestData r
  performRequest :: proxy r -> Request -> RequestData r a -> DOM.JSM a
  default
    performRequest :: forall (proxy :: * -> *) a.
         (RequestData r ~ SomeRequestData r)
      => proxy r -> Request -> RequestData r a -> DOM.JSM a
  performRequest _p req (SomeRequestData p perf) = performRequest p req perf

choose :: forall in_ out a.
     (Client in_, RequestData out ~ SomeRequestData out)
  => (out -> in_) -> RequestData in_ a -> RequestData out a
choose _f perf = SomeRequestData (Proxy @in_) perf

newtype RawRequest a = RawRequest {unRequestDataRaw :: Request -> DOM.JSM a}

instance Client (Raw a) where
  type RequestData (Raw a) = RawRequest
  performRequest _p req (RawRequest f) = f req

instance Client (RawResponse a) where
  type RequestData (RawResponse a) = RawRequest
  performRequest _p req (RawRequest f) = f req

instance (Client next) => Client (End next) where
  type RequestData (End next) = RequestData next
  performRequest _p req perf = performRequest (Proxy @next) req perf

addSegment :: Request -> Text -> Request
addSegment req seg = req{reqSegments = reqSegments req <> DList.singleton seg}

instance (Client next, KnownSymbol seg) => Client (Seg seg next) where
  type RequestData (Seg seg next) = RequestData next
  performRequest _p req perf =
    performRequest (Proxy @next) (addSegment req (fromString (symbolVal (Proxy @seg)))) perf

instance (Client left, Client right) => Client (left :<|> right) where
  type RequestData (left :<|> right) = RequestData left :+: RequestData right
  performRequest _p req = \case
    L1 perf -> performRequest (Proxy @left) req perf
    R1 perf -> performRequest (Proxy @right) req perf

data WhichSeg (segs :: [Symbol]) where
  ThisSeg :: KnownSymbol seg => WhichSeg (seg ': segs)
  ThatSeg :: WhichSeg segs -> WhichSeg (seg ': segs)

thisSeg :: forall seg segs. KnownSymbol seg => WhichSeg (seg ': segs) -> String
thisSeg _ = symbolVal (Proxy @seg)

whichSeg :: WhichSeg segs -> String
whichSeg ts@ThisSeg = thisSeg ts
whichSeg (ThatSeg ws) = whichSeg ws

instance (Client next) => Client (OneOfSegs segs next) where
  type RequestData (OneOfSegs segs next) = WithData (WhichSeg segs) (RequestData next)
  performRequest _p req (WithData ws perf) =
    performRequest (Proxy @next) (addSegment req (fromString (whichSeg ws))) perf

class ToSegment a where
  toSegment :: a -> Text

instance ToSegment Text where
  toSegment = id

data WithData a next b = WithData
  { wdData :: a
  , wdNext :: next b
  }

instance (Client next, ToSegment a) => Client (Capture a next) where
  type RequestData (Capture a next) = WithData a (RequestData next)
  performRequest _p req (WithData x perf) =
    performRequest (Proxy @next) (addSegment req (toSegment x)) perf

instance (Client next, KnownSymbol method) => Client (Method method next) where
  type RequestData (Method seg next) = RequestData next
  performRequest _p req perf = performRequest
    (Proxy @next) req{reqMethod = T.pack (symbolVal (Proxy @method))} perf

data Response a = Response
  { responseStatus :: Word
  , responseBody :: a
  } deriving (Functor, Foldable, Traversable)

data XHRError =
    XHRAborted
  | XHRError
  deriving (Eq, Show)

newtype GetResponse a b = GetResponse {unGetResponse :: Either XHRError (Response a) -> DOM.JSM b}

requestToUri :: Request -> DOM.JSM Text
requestToUri Request{..} = do
#if defined(ghcjs_HOST_OS)
  liftIO $ js_encodeURI $
    "/" <>
    T.intercalate "/" (DList.toList reqSegments) <>
    case DList.toList reqQuery of
      [] -> ""
      query ->
        "?" <> T.intercalate "&"
          [ case mbV of
              Nothing -> k
              Just v -> k <> "=" <> v
          | (k, mbV) <- query
          ]

foreign import javascript unsafe
  "encodeURI($1)"
  js_encodeURI :: Text -> IO Text

#else
  return $ T.decodeUtf8 $ BSL.toStrict $ BB.toLazyByteString $ Http.encodePath
    (DList.toList reqSegments)
    [ (T.encodeUtf8 k, fmap T.encodeUtf8 v)
    | (k, v) <- DList.toList reqQuery
    ]
#endif

performXHR :: forall a. (DOM.XMLHttpRequest -> DOM.JSM a) -> DOM.XMLHttpRequestResponseType -> Request -> DOM.JSM (Either XHRError (Response a))
performXHR getResp respType req@Request{..} = do
  let xhr = reqXHR
  DOM.setResponseType xhr respType
  uri <- requestToUri req
  DOM.open xhr reqMethod uri True reqUser reqPassword
  for_ reqHeaders (uncurry (DOM.setRequestHeader xhr))
  result :: MVar (Either XHRError (Response a)) <- liftIO newEmptyMVar
  let onLoad = lift $ do
        status <- DOM.getStatus xhr
        resp <- getResp xhr
        void (liftIO (tryPutMVar result (Right (Response status resp))))
  let bracketId a b = bracket a id b
  bracketId
    (DOM.Event.on xhr DOM.Event.error (liftIO (void (tryPutMVar result (Left XHRError)))))
    (\_ -> bracketId
      (DOM.Event.on xhr DOM.Event.abortEvent (liftIO (void (tryPutMVar result (Left XHRAborted)))))
      (\_ -> bracketId
        (DOM.Event.on xhr DOM.Event.load onLoad)
        (\_ ->
          onException
            (DOM.XMLHttpRequest.send xhr reqBody >> liftIO (takeMVar result))
            (DOM.XMLHttpRequest.abort xhr))))

#if defined(ghcjs_HOST_OS)

instance (Json.FromJSON a) => Client (JSON a) where
  -- note that we do not decode eagerly because it's often the case that the body
  -- cannot be decoded since web servers return invalid json on errors
  -- (e.g. "Internal server error" on a 500 rather than a json encoded error)
  type RequestData (JSON a) = GetResponse (IO (Either String a))
  performRequest _p req (GetResponse f) = do
    resp <- performXHR DOM.XMLHttpRequest.getResponse DOM.XMLHttpRequestResponseTypeJson req
    f (fmap (fmap (Json.runParser Json.parseJSON)) resp)

instance (Client next, Json.ToJSON a) => Client (ReqBodyJSON a next) where
  type RequestData (ReqBodyJSON a next) = WithData a (RequestData next)
  performRequest _p Request{..} (WithData x perf) = do
    s <- Json.toJSONString =<< Json.toJSON x
    performRequest
      (Proxy @next) Request{reqBody = Just s, ..} perf

#else

instance (Json.FromJSON a) => Client (JSON a) where
  -- note that we do not decode eagerly because it's often the case that the body
  -- cannot be decoded since web servers return invalid json on errors
  -- (e.g. "Internal server error" on a 500 rather than a json encoded error)
  type RequestData (JSON a) = GetResponse (DOM.JSM (Either String a))
  performRequest _p req (GetResponse f) = do
    resp <- performXHR DOM.getResponseTextUnchecked DOM.XMLHttpRequestResponseTypeText req
    f (fmap (fmap (return . Json.eitherDecode . BSL.fromStrict . T.encodeUtf8)) resp)

instance (Client next, Json.ToJSON a) => Client (ReqBodyJSON a next) where
  type RequestData (ReqBodyJSON a next) = WithData a (RequestData next)
  performRequest _p Request{..} (WithData x perf) = do
    let s = JSaddle.toJSString (T.decodeUtf8 (BSL.toStrict (Json.encode x)))
    performRequest
      (Proxy @next) Request{reqBody = Just s, ..} perf

#endif

instance (Client next) => Client (ExtraHeaders next) where
  type RequestData (ExtraHeaders next) = RequestData next
  performRequest _p req perf = performRequest (Proxy @next) req perf

instance (Client next) => Client (NoCache next) where
  type RequestData (NoCache next) = RequestData next
  performRequest _p req perf = performRequest (Proxy @next) req perf

instance (Client next) => Client (WithIO next) where
  type RequestData (WithIO next) = RequestData next
  performRequest _p req perf = performRequest (Proxy @next) req perf

instance (Client next) => Client (ReqBodyMultipart a next) where
  type
    RequestData (ReqBodyMultipart a next) =
      WithData DOM.FormData (RequestData next)
  performRequest _p Request{..} (WithData fd perf) =
    performRequest (Proxy @next) Request{reqBody = Just fd, ..} perf

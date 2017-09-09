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
module Solga.Client.GHCJS where
  {-
  ( Client(..)
  , SomeRequestData(..)
  , choose
  , RawRequest(..)
  , ToSegment(..)
  , WithData(..)
  , GetResponse(..)
  ) where
  -}

import Data.Kind
import Data.Proxy
import GHC.Generics
import GHC.TypeLits (symbolVal, KnownSymbol, Symbol)
import Data.Monoid ((<>))
import qualified JavaScript.Web.XMLHttpRequest as Xhr
import Control.Exception (Exception, throwIO)
import qualified Data.JSString as JSS
import Data.JSString (JSString)
import Data.Typeable (Typeable)
import qualified Data.DList as DList
import Data.DList (DList)
import Data.String (fromString)
import qualified JavaScript.JSON.Types.Class as Json
import qualified JavaScript.JSON.Types.Internal as Json
import GHCJS.Types (Immutable)

import Solga.Core hiding (Header)

data SomeRequestData out a = forall in_. (Client in_) => SomeRequestData (Proxy in_) (RequestData in_ a)

type Header = (JSString, JSString)

data Request = Request
  { reqMethod :: String
  , reqHost :: JSString
  , reqSegments :: DList JSString
  , reqQueryString :: JSString
  , reqData :: Xhr.RequestData
  , reqLogin :: Maybe (JSString, JSString)
  , reqHeaders :: [Header]
  , reqWithCredentials :: Bool
  }

newtype BadMethod = BadMethod String
  deriving (Eq, Show, Typeable)
instance Exception BadMethod

foreign import javascript unsafe
  "encodeURI($1)"
  js_encodeURI :: JSString -> IO JSString

toXhrRequest :: Request -> IO Xhr.Request
toXhrRequest Request{..} = do
  meth <- case reqMethod of
    "GET" -> return Xhr.GET
    "POST" -> return Xhr.POST
    "PUT" -> return Xhr.PUT
    "DELETE" -> return Xhr.DELETE
    x -> throwIO (BadMethod x)
  uri <- js_encodeURI (reqHost <> "/" <> JSS.intercalate "/" (DList.toList reqSegments) <> reqQueryString)
  return Xhr.Request
    { Xhr.reqMethod = meth
    , Xhr.reqURI = uri
    , Xhr.reqLogin = reqLogin
    , Xhr.reqHeaders = reqHeaders
    , Xhr.reqWithCredentials = reqWithCredentials
    , Xhr.reqData = reqData
    }

class Client r where
  type RequestData r :: * -> *
  type RequestData r = SomeRequestData r
  performRequest :: proxy r -> Request -> RequestData r a -> IO a
  default
    performRequest :: forall (proxy :: * -> *) a.
         (RequestData r ~ SomeRequestData r)
      => proxy r -> Request -> RequestData r a -> IO a
  performRequest _p req (SomeRequestData p perf) = performRequest p req perf

choose :: forall in_ out a.
     (Client in_, RequestData out ~ SomeRequestData out)
  => (out -> in_) -> RequestData in_ a -> RequestData out a
choose _f perf = SomeRequestData (Proxy @in_) perf

newtype RawRequest a = RawRequest {unRequestDataRaw :: Request -> IO a}

instance Client (Raw a) where
  type RequestData (Raw a) = RawRequest
  performRequest _p req (RawRequest f) = f req

instance Client (RawResponse a) where
  type RequestData (RawResponse a) = RawRequest
  performRequest _p req (RawRequest f) = f req

instance (Client next) => Client (End next) where
  type RequestData (End next) = RequestData next
  performRequest _p req perf = performRequest (Proxy @next) req perf

addSegment :: Request -> JSString -> Request
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
  toSegment :: a -> JSString

instance ToSegment JSString where
  toSegment = id

data WithData a next b = WithData
  { ardData :: a
  , ardNext :: next b
  }

instance (Client next, ToSegment a) => Client (Capture a next) where
  type RequestData (Capture a next) = WithData a (RequestData next)
  performRequest _p req (WithData x perf) =
    performRequest (Proxy @next) (addSegment req (toSegment x)) perf

instance (Client next, KnownSymbol method) => Client (Method method next) where
  type RequestData (Method seg next) = RequestData next
  performRequest _p req perf = performRequest
    (Proxy @next) req{reqMethod = symbolVal (Proxy @method)} perf

newtype GetResponse resp a b = GetResponse {unGetResponse :: Xhr.Response resp -> a -> IO b}

instance (Json.FromJSON a) => Client (JSON a) where
  -- note that we do not decode eagerly because it's often the case that the body
  -- cannot be decoded since web servers return invalid json on errors
  -- (e.g. "Internal server error" on a 500 rather than a json encoded error)
  type RequestData (JSON a) = GetResponse (Json.SomeValue Immutable) (Maybe (Either String a))
  performRequest _p req (GetResponse f) = do
    resp <- Xhr.xhr =<< toXhrRequest req
    f resp $ do
      data_ <- Xhr.contents resp
      return (Json.parseEither Json.parseJSON data_)

instance (Client next) => Client (ExtraHeaders next) where
  type RequestData (ExtraHeaders next) = RequestData next
  performRequest _p req perf = performRequest (Proxy @next) req perf

instance (Client next) => Client (NoCache next) where
  type RequestData (NoCache next) = RequestData next
  performRequest _p req perf = performRequest (Proxy @next) req perf

instance (Client next, Json.ToJSON a) => Client (ReqBodyJSON a next) where
  type RequestData (ReqBodyJSON a next) = WithData a (RequestData next)
  performRequest _p req (WithData x perf) = performRequest
    (Proxy @next) req{reqData = Xhr.StringData (Json.encode (Json.toJSON x))} perf

instance (Client next) => Client (WithIO next) where
  type RequestData (WithIO next) = RequestData next
  performRequest _p req perf = performRequest (Proxy @next) req perf

instance (Client next) => Client (ReqBodyMultipart fp a next) where
  type
    RequestData (ReqBodyMultipart fp a next) =
      WithData [(JSString, Xhr.FormDataVal)] (RequestData next)
  performRequest _p req (WithData fd perf) = do
    performRequest (Proxy @next) req{reqData = Xhr.FormData fd} perf

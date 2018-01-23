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
{-# LANGUAGE UndecidableInstances #-}
module Solga.Client
  ( Client(..)
  , SomeRequestData(..)
  , choose
  , RawRequest(..)
  , ToSegment(..)
  , WithData(..)
  , GetResponse(..)
  ) where

import Data.Kind
import Data.Proxy
import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.MultipartFormData as Http
import GHC.Generics
import qualified Data.ByteString.Char8 as BSC8
import GHC.TypeLits (symbolVal, KnownSymbol, Symbol)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import qualified Data.Text.Encoding as T
import Data.Text (Text)
import Network.HTTP.Types (urlEncodeBuilder)
import qualified Blaze.ByteString.Builder as Blaze
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

import Solga.Core

data SomeRequestData out a = forall in_. (Client in_) => SomeRequestData (Proxy in_) (RequestData in_ a)

class Client r where
  type RequestData r :: * -> *
  type RequestData r = SomeRequestData r
  performRequest :: proxy r -> Http.Request -> Http.Manager -> RequestData r a -> IO a
  default
    performRequest :: forall (proxy :: * -> *) a.
         (RequestData r ~ SomeRequestData r)
      => proxy r -> Http.Request -> Http.Manager -> RequestData r a -> IO a
  performRequest _p req mgr (SomeRequestData p perf) = performRequest p req mgr perf

choose :: forall in_ out a.
     (Client in_, RequestData out ~ SomeRequestData out)
  => (out -> in_) -> RequestData in_ a -> RequestData out a
choose _f perf = SomeRequestData (Proxy @in_) perf

newtype RawRequest a = RawRequest {unRequestDataRaw :: Http.Request -> Http.Manager -> IO a}

instance Client (Raw a) where
  type RequestData (Raw a) = RawRequest
  performRequest _p mgr req (RawRequest f) = f mgr req

instance Client (RawResponse a) where
  type RequestData (RawResponse a) = RawRequest
  performRequest _p mgr req (RawRequest f) = f mgr req

instance (Client next) => Client (End next) where
  type RequestData (End next) = RequestData next
  performRequest _p mgr req perf = performRequest (Proxy @next) mgr req perf

addSegment :: Http.Request -> Text -> Http.Request
addSegment req segtxt = req
  { Http.path = if BS.null (Http.path req) || BSC8.last (Http.path req) == '/'
      then Http.path req <> seg
      else Http.path req <> "/" <> seg
  }
  where
    seg = BSL.toStrict (Blaze.toLazyByteString (urlEncodeBuilder False (T.encodeUtf8 segtxt)))

instance (Client next, KnownSymbol seg) => Client (Seg seg next) where
  type RequestData (Seg seg next) = RequestData next
  performRequest _p req mgr perf =
    performRequest (Proxy @next) (addSegment req (T.pack (symbolVal (Proxy @seg)))) mgr perf

instance (Client left, Client right) => Client (left :<|> right) where
  type RequestData (left :<|> right) = RequestData left :+: RequestData right
  performRequest _p mgr req = \case
    L1 perf -> performRequest (Proxy @left) mgr req perf
    R1 perf -> performRequest (Proxy @right) mgr req perf

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
  performRequest _p req mgr (WithData ws perf) =
    performRequest (Proxy @next) (addSegment req (T.pack (whichSeg ws))) mgr perf

class ToSegment a where
  toSegment :: a -> Text

instance ToSegment Text where
  toSegment = id

data WithData a next b = WithData
  { ardData :: a
  , ardNext :: next b
  }

instance (Client next, ToSegment a) => Client (Capture a next) where
  type RequestData (Capture a next) = WithData a (RequestData next)
  performRequest _p req mgr (WithData x perf) =
    performRequest (Proxy @next) (addSegment req (toSegment x)) mgr perf

instance (Client next, KnownSymbol method) => Client (Method method next) where
  type RequestData (Method method next) = RequestData next
  performRequest _p req mgr perf = performRequest
    (Proxy @next) req{Http.method = BSC8.pack (symbolVal (Proxy @method))} mgr perf

newtype GetResponse resp a b = GetResponse {unGetResponse :: Http.Response resp -> a -> IO b}

instance (Aeson.FromJSON a) => Client (JSON a) where
  -- note that we do not decode eagerly because it's often the case that the body
  -- cannot be decoded since web servers return invalid json on errors
  -- (e.g. "Internal server error" on a 500 rather than a json encoded error)
  type RequestData (JSON a) = GetResponse BSL.ByteString (Either String a)
  performRequest _p req mgr (GetResponse f) = do
    resp <- Http.httpLbs req mgr
    let decode = Aeson.eitherDecode' (Http.responseBody resp)
    f resp decode

instance (Client next) => Client (ExtraHeaders next) where
  type RequestData (ExtraHeaders next) = RequestData next
  performRequest _p req mgr perf = performRequest (Proxy @next) req mgr perf

instance (Client next) => Client (NoCache next) where
  type RequestData (NoCache next) = RequestData next
  performRequest _p req mgr perf = performRequest (Proxy @next) req mgr perf

instance (Client next) => Client (RedirectOnTrailingSlash next) where
  type RequestData (RedirectOnTrailingSlash next) = RequestData next
  performRequest _p req mgr perf = performRequest (Proxy @next) req mgr perf

instance (Client next, Aeson.ToJSON a) => Client (ReqBodyJSON a next) where
  type RequestData (ReqBodyJSON a next) = WithData a (RequestData next)
  performRequest _p req mgr (WithData x perf) = performRequest
    (Proxy @next) req{Http.requestBody = Http.RequestBodyLBS (Aeson.encode x)} mgr perf

instance (Client next) => Client (WithIO next) where
  type RequestData (WithIO next) = RequestData next
  performRequest _p req mgr perf = performRequest (Proxy @next) req mgr perf

instance (Client next) => Client (ReqBodyMultipart a next) where
  type
    RequestData (ReqBodyMultipart a next) =
      WithData (a, a -> ([Http.Part], Maybe ByteString)) (RequestData next)
  performRequest _p req mgr (WithData (x, f) perf) = do
    let (parts, mbBoundary) = f x
    req' <- case mbBoundary of
      Nothing -> Http.formDataBody parts req
      Just x -> Http.formDataBodyWithBoundary x parts req
    performRequest (Proxy @next) req' mgr perf


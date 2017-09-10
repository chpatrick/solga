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
module Solga.Core
  ( -- * Path components
    type (:>), type (/>)
  , Get
  , Post
  , JSON(..)
  , Raw(..)
  , RawResponse(..)
  , End(..)
  , WithIO(..)
  , Seg(..)
  , OneOfSegs(..)
  , Capture(..)
  , Method(..)
  , HeaderName
  , Header
  , ResponseHeaders
  , ExtraHeaders(..)
  , NoCache(..)
  , ReqBodyJSON(..)
  , MultiPartParam
  , MultiPartFile
  , MultiPartFileInfo(..)
  , MultiPartData
  , ReqBodyMultipart(..)
  , Endpoint
  , (:<|>)(..)
  ) where

import           GHC.TypeLits
import           Data.ByteString (ByteString)
import           Data.CaseInsensitive (CI)

---------------------------------------------------

-- | Compose routers. This is just type application,
-- ie.: @Foo :> Bar :> Baz == Foo (Bar Baz)@
type f :> g = f g
infixr 2 :>

-- | Serve a given WAI `Wai.Application`.
newtype Raw a = Raw { rawApp :: a }

-- | Serve a given WAI `Wai.Response`.
newtype RawResponse a = RawResponse { rawResponse :: a }

-- | Only accept the end of a path.
newtype End next = End { endNext :: next }

-- | Match a constant directory in the path.
--
--   When specifying APIs, use the `/>` combinator to specify sub-paths:
--   @"foo" `/>` `JSON` Bar@
newtype Seg (seg :: Symbol) next = Seg { segNext :: next }
  deriving (Eq, Ord, Show)

-- | Match a path, segment, e.g @"foo" `/>` `JSON` Bar@
type seg /> g = Seg seg :> g
infixr 2 />

-- | Try to route with @left@, or try to route with @right@.
data left :<|> right = (:<|>) { altLeft :: left, altRight :: right }
  deriving (Eq, Ord, Show)

infixr 1 :<|>

-- | Match any of a set of path segments.
data OneOfSegs (segs :: [ Symbol ]) next = OneOfSegs { oneOfSegsNext :: next }

-- | Capture a path segment and pass it on.
newtype Capture a next = Capture { captureNext :: a -> next }

-- | Accepts requests with a certain method.
newtype Method (method :: Symbol) next = Method { methodNext :: next }
  deriving (Eq, Ord, Show)

-- | Return a given JSON object
newtype JSON a = JSON { jsonResponse :: a }
  deriving (Eq, Ord, Show)

type HeaderName = CI ByteString
type Header = (HeaderName, ByteString)
type ResponseHeaders = [Header]

-- | Set extra headers on responses.
-- Existing headers will be overriden if specified here.
data ExtraHeaders next = ExtraHeaders
  { extraHeaders :: ResponseHeaders
  , extraHeadersNext :: next
  }

-- | Prevent caching for sub-routers.
newtype NoCache next = NoCache { noCacheNext :: next }

-- | Parse a JSON request body.
newtype ReqBodyJSON a next = ReqBodyJSON { reqBodyJSONNext :: a -> next }

-- | Produce a response with `IO`.
newtype WithIO next = WithIO { withIONext :: IO next }

type MultiPartParam = (ByteString, ByteString)
type MultiPartFile y = (ByteString, MultiPartFileInfo y)

data MultiPartFileInfo c = MultiPartFileInfo
  { mpfiName :: ByteString
  , mpfiContentType :: ByteString
  , mpfiContent :: FilePath
  }

-- | A parsed "multipart/form-data" request.
type MultiPartData y = ([MultiPartParam], [MultiPartFile y])

-- | Accept a "multipart/form-data" request.
-- Files will be stored in a temporary directory and will be deleted
-- automatically after the request is processed.
data ReqBodyMultipart y a next = ReqBodyMultipart
  { reqMultiPartParse :: MultiPartData y -> Either String a
  , reqMultiPartNext :: a -> next
  }

-- | Useful synonym for dynamic endpoints: accept requests with a given method, compute a JSON response in `IO` and don't cache.
type Endpoint method a = End :> NoCache :> Method method :> WithIO :> a

-- | Handle a "GET" request and produce a "JSON" response, with `IO`.
type Get a = Endpoint "GET" (JSON a)
-- | Handle a "POST" request and produce a "JSON" response, with `IO`.
type Post a = Endpoint "POST" (JSON a)


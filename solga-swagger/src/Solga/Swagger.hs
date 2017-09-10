{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Solga.Swagger
  ( genSwagger
  , RouterSwagger(..)
  -- * Implementation
  , GenPathsM
  , Paths
  , Context(..)
  , passPaths
  , noPaths
  ) where

import           Control.Monad.State
import           Control.Monad.Except
import qualified Network.HTTP.Types as HTTP

import           Control.Lens hiding (Context)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.DList as DL
import qualified Data.HashMap.Strict as HMS
import qualified Data.HashMap.Strict.InsOrd as OHMS
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Typeable
import           GHC.Generics
import           GHC.TypeLits
import           Data.Swagger as Swagger
import           Data.Swagger.Declare

import           Solga.Core

data Context = Context
  { contextMethod :: Maybe HTTP.Method -- ^ Any method currently set.
  , pathSegments :: DL.DList Text -- ^ The current path.
  , operationContext :: Operation -- ^ The current template operation.
  , paramScope :: HMS.HashMap Text Int -- ^ The parameter names in use.
  } deriving (Show)

noContext :: Context
noContext = Context
  { contextMethod = mempty
  , pathSegments = mempty
  , operationContext = mempty
  , paramScope = mempty
  }

type GenPathsM = ExceptT ( Text, Context ) (Declare (Definitions Schema))
type Paths = OHMS.InsOrdHashMap Text PathItem

-- | A type for which we can generate a Swagger specification.
class RouterSwagger r where
  genPaths :: Proxy r -> Context -> GenPathsM Paths
  default genPaths :: (RouterSwagger (Rep r ())) => Proxy r -> Context -> GenPathsM Paths
  genPaths _ = genPaths (Proxy :: Proxy (Rep r ()))

-- | For a Router @f next@, produce the same paths as @next@ without modification.
passPaths :: (r ~ f next, RouterSwagger next) => Proxy r -> Context -> GenPathsM Paths
passPaths p = genPaths (nextProxy p)

-- | Produce no paths.
noPaths :: Proxy r -> Context -> GenPathsM Paths
noPaths _ _ = return mempty

-- | Generate a Swagger specification for a given type.
genSwagger :: RouterSwagger r => Proxy r -> Either ( Text, Context ) Swagger
genSwagger p = case runDeclare (runExceptT (genPaths p noContext)) mempty of
  ( _, Left err ) -> Left err
  ( defs, Right ps ) ->
    let
      fpPaths = OHMS.fromList $ map (\(k, v) -> ( T.unpack k, v )) $ OHMS.toList ps
    in Right (mempty & paths .~ fpPaths & definitions .~ defs)

nextProxy :: Proxy (r next) -> Proxy next
nextProxy _ = Proxy

pathsFromContext :: Response -> Context -> GenPathsM Paths
pathsFromContext response ctx@Context { contextMethod, pathSegments, operationContext } = do
  let path = foldMap (\seg -> "/" <> seg) (DL.toList pathSegments)
  methodSetter <- case contextMethod of
    Just m -> case m of
      "GET" -> return Swagger.get
      "PUT" -> return Swagger.put
      "POST" -> return Swagger.post
      "DELETE" -> return Swagger.delete
      "OPTIONS" -> return Swagger.options
      "HEAD" -> return Swagger.head_
      "PATCH" -> return Swagger.patch
      _ -> throwError ( "Unsupported method " <> decodeUtf8 m, ctx )
    _ -> throwError ( "Missing method in context.", ctx )
  let resps = mempty & responses .~ OHMS.singleton 200 (Inline response)
  let operation = operationContext & responses .~ resps
  let pathItem = mempty & methodSetter ?~ operation
  return $ OHMS.singleton path pathItem

instance RouterSwagger (RawResponse a) where
  genPaths _ = pathsFromContext mempty

instance ToSchema a => RouterSwagger (JSON a) where
  genPaths p ctx = do
    respSchemaRef <- lift $ declareSchemaRef (nextProxy p)
    let resp = mempty & schema ?~ respSchemaRef
    pathsFromContext resp ctx

instance (KnownSymbol m, RouterSwagger next) => RouterSwagger (Method m next) where
  genPaths p ctx = case ctx of
    Context { contextMethod = Just ctxMeth } | ctxMeth /= method -> throwError ( "Conflicting method specification.", ctx )
    _ -> genPaths (nextProxy p) ctx { contextMethod = Just method }
    where
      method = BSC.pack (symbolVal (Proxy :: Proxy m))

instance (KnownSymbol seg, RouterSwagger next) => RouterSwagger (Seg seg next) where
  genPaths p ctx = do
    let seg = T.pack $ symbolVal (Proxy :: Proxy seg)
    genPaths (nextProxy p) ctx { pathSegments = pathSegments ctx `DL.snoc` seg }

instance RouterSwagger next => RouterSwagger (WithIO next) where
  genPaths = passPaths
instance RouterSwagger next => RouterSwagger (End next) where
  genPaths = passPaths
instance RouterSwagger next => RouterSwagger (NoCache next) where
  genPaths = passPaths
instance RouterSwagger next => RouterSwagger (ExtraHeaders next) where
  genPaths = passPaths

instance RouterSwagger (ReqBodyMultipart a next) where
  genPaths = noPaths

instance RouterSwagger (OneOfSegs '[] next) where
  genPaths = noPaths

instance (KnownSymbol seg, RouterSwagger next, RouterSwagger (OneOfSegs segs next)) => RouterSwagger (OneOfSegs (seg ': segs) next) where
  genPaths p ctx = do
    let seg = T.pack $ symbolVal (Proxy :: Proxy seg)
    nextPaths <- genPaths (nextProxy p) ctx { pathSegments = pathSegments ctx `DL.snoc` seg }
    nextSegPaths <- genPaths (Proxy :: Proxy (OneOfSegs segs next)) ctx
    return (nextPaths `OHMS.union` nextSegPaths)

instance RouterSwagger (Raw a) where
  genPaths = noPaths

instance (RouterSwagger left, RouterSwagger right) => RouterSwagger (left :<|> right) where
  genPaths _ ctx =
    OHMS.unionWith mappend
      <$> genPaths (Proxy :: Proxy left) ctx
      <*> genPaths (Proxy :: Proxy right) ctx

-- Generic paths
instance RouterSwagger r => RouterSwagger (K1 i r p) where
  genPaths _ = genPaths (Proxy :: Proxy r)

instance RouterSwagger (f p) => RouterSwagger (M1 i c f p) where
  genPaths _ = genPaths (Proxy :: Proxy (f p))

instance (RouterSwagger (left p), RouterSwagger (right p)) => RouterSwagger ((left :*: right) p) where
  genPaths _ ctx =
    OHMS.unionWith mappend
      <$> genPaths (Proxy :: Proxy (left p)) ctx
      <*> genPaths (Proxy :: Proxy (right p)) ctx

instance (ToSchema a, RouterSwagger next) => RouterSwagger (ReqBodyJSON a next) where
  genPaths p ctx@Context { operationContext } = do
    let hasOtherBody = notNullOf (parameters . folded . _Inline . schema . _ParamBody) operationContext
    if hasOtherBody
      then throwError ( "Conflicting request body schemas.", ctx )
      else do
        bodySchemaRef <- lift $ declareSchemaRef (Proxy :: Proxy a)
        let param = mempty & name .~ "requestBody" & required .~ Just True & schema .~ (ParamBody bodySchemaRef)
        genPaths (nextProxy p) ctx { operationContext = operationContext & parameters <>~ [ Inline param ] }

newName :: Text -> Context -> ( Text, Context )
newName desiredName ctx@Context { paramScope } = case HMS.lookup desiredName paramScope of
  Nothing -> ( desiredName, ctx { paramScope = HMS.insert desiredName 1 paramScope } )
  Just count -> let newCount = count + 1 in ( desiredName <> T.pack (show newCount), ctx { paramScope = HMS.insert desiredName newCount paramScope } )

instance (Typeable a, ToParamSchema a, RouterSwagger next) => RouterSwagger (Capture a next) where
  genPaths p ctx = do
    let desiredName = T.pack $ tyConName $ typeRepTyCon $ typeRep (Proxy :: Proxy a)
    let ( paramName, newCtx ) = newName desiredName ctx
    let pSchema = toParamSchema (Proxy :: Proxy a)
    let pOtherSchema = mempty & in_ .~ ParamPath & paramSchema .~ pSchema
    let param = mempty & name .~ paramName & required .~ Just True & schema .~ ParamOther pOtherSchema
    genPaths (nextProxy p) newCtx
      { pathSegments  = pathSegments ctx `DL.snoc` paramName
      , operationContext = operationContext newCtx & parameters <>~ [ Inline param ]
      }

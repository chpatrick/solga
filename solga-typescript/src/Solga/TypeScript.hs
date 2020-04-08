{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Solga.TypeScript
  ( Info(..)
  , Paths(..)
  , TypeScriptRoute(..)
  , typeScript
  ) where

import qualified Data.Aeson.TypeScript.TH as Aeson
import qualified Data.Aeson.TypeScript.Recursive as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HMS
import Solga.Core
import Data.Maybe (isJust, fromMaybe)
import GHC.TypeLits
import Data.Proxy
import GHC.Generics
import qualified Data.DList as DL
import Data.List (foldl')
import Data.Monoid ((<>))
import Control.Monad (guard)
import qualified Data.Set as S

{-
export class TypeScriptRoute {
  private http: { fetch(url: RequestInfo, init?: RequestInit): Promise<Response> };
  private baseUrl: string;

  constructor(baseUrl?: string, http?: { fetch(url: RequestInfo, init?: RequestInit): Promise<Response> }) {
    this.http = http ? http : <any>window;
    this.baseUrl = baseUrl ? baseUrl : "";
  }
}
-}

{-
data Foo a b = Foo { foo1 :: a, foo2 :: b }

deriveTypeScript (defaultOptions {fieldLabelModifier = drop 3, constructorTagModifier = map toLower}) ''Foo

data Bar = Bar (Foo Int Bool) Int

deriveTypeScript (defaultOptions {fieldLabelModifier = drop 3, constructorTagModifier = map toLower}) ''Bar
-}

-- The strategy is to build a single TypeScript type to represent all the possible
-- requests that we can do.

data Info = Info
  { infoReqJSON :: Maybe T.Text
  , infoReqMultiPart :: Bool
  , infoRespJSON :: Maybe T.Text
  , infoRespRaw :: Bool
  , infoMethod :: Maybe T.Text
  } deriving (Eq, Show)

data Paths =
    PathsCapture (DL.DList Paths)
  | PathsMatch [Text] (DL.DList Paths) -- match any of these
  | PathsEnd Info
  | PathsNothing
  deriving (Eq, Show)

generateTypeScript :: forall a. (TypeScriptRoute a) => Proxy a -> Either String (DL.DList Paths, S.Set Aeson.TSType)
generateTypeScript _ = typeScriptRoute (Proxy @a) Info
  { infoReqJSON = Nothing
  , infoReqMultiPart = False
  , infoRespJSON = Nothing
  , infoRespRaw = False
  , infoMethod = Nothing
  }

class TypeScriptRoute a where
  typeScriptRoute :: Proxy a -> Info -> Either String (DL.DList Paths, S.Set Aeson.TSType)
  default typeScriptRoute :: (TypeScriptRoute (Rep a ())) => Proxy a -> Info -> Either String (DL.DList Paths, S.Set Aeson.TSType)
  typeScriptRoute _ = typeScriptRoute (Proxy @(Rep a ()))

instance TypeScriptRoute (Raw a) where
  typeScriptRoute _ _info = return mempty

instance (TypeScriptRoute a) => TypeScriptRoute (End a) where
  typeScriptRoute _ = typeScriptRoute (Proxy @a)

instance TypeScriptRoute (RawResponse a) where
  typeScriptRoute _ info = return (pure (PathsEnd info{infoRespRaw = True}), mempty)

instance (Aeson.TypeScript a) => TypeScriptRoute (JSON a) where
  typeScriptRoute _ info = return
    ( pure (PathsEnd info{infoRespJSON = Just (T.pack (Aeson.getTypeScriptType (Proxy @a)))})
    , S.singleton (Aeson.TSType (Proxy @a))
    )

instance (KnownSymbol seg, TypeScriptRoute next) => TypeScriptRoute (Seg seg next) where
  typeScriptRoute _ info = do
    (paths, types) <- typeScriptRoute (Proxy @next) info
    return (pure (PathsMatch [T.pack (symbolVal (Proxy :: Proxy seg))] paths), types)

instance (TypeScriptRoute left, TypeScriptRoute right) => TypeScriptRoute (left :<|> right) where
  typeScriptRoute _ info = mappend <$> typeScriptRoute (Proxy @left) info <*> typeScriptRoute (Proxy @right) info

class SymbolList (a :: [Symbol]) where
  symbolList :: Proxy a -> [T.Text]

instance SymbolList '[] where
  symbolList _ = []

instance (KnownSymbol seg, SymbolList segs) => SymbolList (seg ': segs) where
  symbolList _ = T.pack (symbolVal (Proxy @seg)) : symbolList (Proxy @segs)

instance (SymbolList segs, TypeScriptRoute next) => TypeScriptRoute (OneOfSegs segs next) where
  typeScriptRoute _ info = do
    let segs = symbolList (Proxy @segs)
    (paths, types) <- typeScriptRoute (Proxy @next) info
    return (pure (PathsMatch segs paths), types)

instance (TypeScriptRoute next) => TypeScriptRoute (Capture a next) where
  typeScriptRoute _ info = do
    (paths, types) <- typeScriptRoute (Proxy @next) info
    return (pure (PathsCapture paths), types)

instance (TypeScriptRoute next, KnownSymbol method) => TypeScriptRoute (Method method next) where
  typeScriptRoute _ info = case infoMethod info of
    Nothing -> typeScriptRoute (Proxy @next) info{infoMethod = Just (T.pack (symbolVal (Proxy @method)))}
    Just{} -> Left "Method set multiple times!"

instance (TypeScriptRoute next) => TypeScriptRoute (ExtraHeaders next) where
  typeScriptRoute _ = typeScriptRoute (Proxy @next)

instance (TypeScriptRoute next) => TypeScriptRoute (NoCache next) where
  typeScriptRoute _ = typeScriptRoute (Proxy @next)

instance (TypeScriptRoute next, Aeson.TypeScript a) => TypeScriptRoute (ReqBodyJSON a next) where
  typeScriptRoute _ info = case infoReqJSON info of
    Just{} -> Left "Req body set multiple times!"
    Nothing -> do
      (paths, types) <- typeScriptRoute (Proxy @next) info{infoReqJSON = Just (T.pack (Aeson.getTypeScriptType (Proxy @a)))}
      return (paths, S.insert (Aeson.TSType (Proxy @a)) types)

instance (TypeScriptRoute next) => TypeScriptRoute (WithIO next) where
  typeScriptRoute _ = typeScriptRoute (Proxy @next)

instance (TypeScriptRoute next) => TypeScriptRoute (ReqBodyMultipart a next) where
  typeScriptRoute _ info = if infoReqMultiPart info
    then Left "Req body set multiple times!"
    else typeScriptRoute (Proxy @next) info{infoReqMultiPart = True}

instance (TypeScriptRoute next) => TypeScriptRoute (WithReferer next) where
  typeScriptRoute _ = typeScriptRoute (Proxy @next)

-- Generic
-- --------------------------------------------------------------------

instance TypeScriptRoute r => TypeScriptRoute (K1 i r p) where
  typeScriptRoute _ = typeScriptRoute (Proxy @r)

instance TypeScriptRoute (f p) => TypeScriptRoute (M1 i c f p) where
  typeScriptRoute _ = typeScriptRoute (Proxy :: Proxy (f p))

instance (TypeScriptRoute (left p), TypeScriptRoute (right p)) => TypeScriptRoute ((left :*: right) p) where
  typeScriptRoute _ info = mappend <$> typeScriptRoute (Proxy @(left p)) info <*> typeScriptRoute (Proxy @(right p)) info

-- To hide from TypeScript
-- --------------------------------------------------------------------

instance (TypeScriptRoute next) => TypeScriptRoute (Hidden next) where
  typeScriptRoute _ _ = return (pure PathsNothing, mempty)

-- Computing the typescript stuff
-- --------------------------------------------------------------------

-- env.seg("blah").param("foo").send("json-body");

data TypeScriptReq
  = TSRMultipart
  | TSRJson T.Text
  | TSRNoBody

data TypeScriptSend = TypeScriptSend
  { tssMethod :: T.Text
  , tssReq :: TypeScriptReq
  , tssResp :: T.Text
  }

data TypeScriptDict = TypeScriptDict
  { tsdSegments :: HMS.HashMap T.Text TypeScriptDict
  , tsdCapture :: Maybe TypeScriptDict
  , tsdSend :: Maybe TypeScriptSend
  }

data TypeScriptSeg
  = TSSConst T.Text
  | TSSVar T.Text

typeScript :: (TypeScriptRoute a) => Proxy a -> T.Text -> T.Text
typeScript p name = case generateTypeScript p of
  Left err -> error err
  Right (paths, types) -> let
    dict = go paths
    in T.unlines
      [ T.pack $ Aeson.formatTSDeclarations'
          Aeson.defaultFormattingOptions{ Aeson.exportTypes = True }
          (do
            Aeson.TSType typ <- S.toList (Aeson.getTransitiveClosure types)
            Aeson.getTypeScriptDeclarations typ)
      , ""
      , "export function " <> name <> "(baseUrl: string, send: {send<A>(url: string, method: string): Promise<A>, sendJson<A, B>(url: string, method: string, req: A): Promise<B>, sendForm<A>(url: string, method: string, req: FormData): Promise<A>}): " <> renderDictType dict <> " { return " <> renderDictExpr "send" "baseUrl" [] dict <> "; }"
      ]
  where
    emptyDict = TypeScriptDict{ tsdSegments = mempty, tsdCapture = Nothing, tsdSend = Nothing }

    go paths = goFields emptyDict (DL.toList paths)

    goFields dict = \case
      [] -> dict
      path : paths -> goFields (pathToField dict path) paths

    dictNonEmpty dict = isJust (tsdSend dict) || isJust (tsdCapture dict) || HMS.size (tsdSegments dict) > 0

    mergeDicts dict1 dict2 = TypeScriptDict
      { tsdCapture = case (tsdCapture dict1, tsdCapture dict2) of
          (Nothing, Nothing) -> Nothing
          (Just x, Nothing) -> x <$ guard (dictNonEmpty x)
          (Nothing, Just x) -> x <$ guard (dictNonEmpty x)
          (Just x, Just y) -> let z = mergeDicts x y in z <$ guard (dictNonEmpty z)
      , tsdSegments = HMS.filter dictNonEmpty (HMS.unionWith mergeDicts (tsdSegments dict1) (tsdSegments dict2))
      , tsdSend = case (tsdSend dict1, tsdSend dict2) of
          (Nothing, Nothing) -> Nothing
          (Just x, Nothing) -> Just x
          (Nothing, Just x) -> Just x
          (Just{}, Just{}) -> error "Conflicting sends"
      }

    pathToField dict0 = \case
      PathsCapture paths -> mergeDicts dict0 (emptyDict { tsdCapture = Just (go paths) })
      PathsEnd info -> if infoRespRaw info
        then dict0
        else let
          req = case (infoReqMultiPart info, infoReqJSON info) of
            (True, Nothing) -> TSRMultipart
            (False, Just j) -> TSRJson j
            (True, Just{}) -> error "Got both multipart and json req body"
            (False, Nothing) -> TSRNoBody
          in mergeDicts dict0 emptyDict{ tsdSend = Just TypeScriptSend{ tssMethod = fromMaybe "GET" (infoMethod info), tssReq = req, tssResp = fromMaybe "void" (infoRespJSON info) } }
      PathsMatch segs paths -> let
        pathsDict = go paths
        in foldl' (\dict seg -> mergeDicts dict emptyDict{ tsdSegments = HMS.singleton seg pathsDict }) dict0 segs
      PathsNothing -> dict0

    renderDictType dict = T.concat $ concat
      [ ["{"]
      , case tsdSend dict of
          Nothing -> []
          Just TypeScriptSend{..} ->
            [ "\"send\": (" <>
              (case tssReq of
                TSRJson j -> "req: " <> j
                TSRMultipart -> "req: FormData"
                TSRNoBody -> "") <>
              ") => Promise<" <> tssResp <> ">"
            ]
      , case tsdCapture dict of
          Nothing -> []
          Just ty -> ["\"param\": (p: string) => " <> renderDictType ty <> ", "]
      , if HMS.size (tsdSegments dict) > 0 then ["\"route\": " <> renderRoutesTypes (tsdSegments dict)] else []
      , ["}"]
      ]

    renderRoutesTypes segs = T.concat $ concat
      [ ["{"]
      , do
          (seg, ty) <- HMS.toList segs
          return (T.pack (show seg) <> ": " <> renderDictType ty <> ", ")
      , ["}"]
      ]

    renderDictExpr sendVar baseUrlVar segs dict = T.concat $ concat
      [ ["{"]
      , case tsdSend dict of
          Nothing -> []
          Just TypeScriptSend{..} -> let
            segToExpr = \case
              TSSConst c -> T.pack (show c)
              TSSVar v -> v
            urlExpr = baseUrlVar <> " + [" <> T.intercalate ", " (map segToExpr (reverse segs)) <> "].join('/')"
            in
              [ "\"send\": (" <>
                (case tssReq of
                  TSRJson j -> "req: " <> j
                  TSRMultipart -> "req: FormData"
                  TSRNoBody -> "") <>
                "): Promise<" <> tssResp <> "> => { return " <>
                (case tssReq of
                  TSRJson{} -> sendVar <> ".sendJson(" <> urlExpr <> ", " <> T.pack (show tssMethod) <> ", req)"
                  TSRMultipart{} -> sendVar <> ".sendForm(" <> urlExpr <> ", " <> T.pack (show tssMethod) <> ", req)"
                  TSRNoBody{} -> sendVar <> ".send(" <> urlExpr <> ", " <> T.pack (show tssMethod) <> ")") <> "; },"
              ]
      , case tsdCapture dict of
          Nothing -> []
          Just ty -> let
            v = "param" <> T.pack (show (length segs))
            in ["\"param\": (" <> v <> ": string): " <> renderDictType ty <> " => { return " <> renderDictExpr sendVar baseUrlVar (TSSVar v : segs) ty <> "; }, "]
      , if HMS.size (tsdSegments dict) > 0 then ["\"route\": " <> renderRoutesExprs sendVar baseUrlVar segs (tsdSegments dict)] else []
      , ["}"]
      ]

    renderRoutesExprs sendVar baseUrlVar segs newSegs = T.concat $ concat
      [ ["{"]
      , do
          (seg, ty) <- HMS.toList newSegs
          return (T.pack (show seg) <> ": " <> renderDictExpr sendVar baseUrlVar (TSSConst seg : segs) ty <> ", ")
      , ["}"]
      ]

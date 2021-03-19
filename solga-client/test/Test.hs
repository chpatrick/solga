{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -ddump-deriv #-}

module Main (main) where

import           Test.Hspec
import           Test.QuickCheck (genericShrink, property, ioProperty, Arbitrary(..), Gen, sized, oneof, scale, listOf)

import           Data.Aeson hiding (json)
import           Data.Hashable
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V
import           Data.Traversable
import           GHC.Generics (Generic)
import           Network.HTTP.Types
import qualified Network.HTTP.Client as Http
import           Data.Proxy (Proxy(..))
import qualified Network.Wai.Handler.Warp as Warp

import           Solga.Core
import           Solga.Router
import           Solga.Client

main :: IO ()
main = do
  Warp.withApplication (return (serve testAPI)) (hspec . spec)

data TestAPI = TestAPI
  { basic :: "basic" /> Get T.Text
  , echoJSON :: "echo-json" /> ReqBodyJSON Value :> Post Value
  , internalError :: "fubar" /> Get T.Text
  , echoCapture :: "echo-capture" /> Capture T.Text :> Get T.Text
  } deriving (Generic)
instance Router TestAPI
instance Client TestAPI

testAPI :: TestAPI
testAPI = TestAPI
  { basic = brief (return "basic")
  , echoJSON = brief return
  , internalError = brief (error "quality programming")
  , echoCapture = brief return
  }

req :: Warp.Port -> RequestData TestAPI a -> IO a
req p x = do
  mgr <- Http.newManager Http.defaultManagerSettings
  performRequest (Proxy @TestAPI) Http.defaultRequest{Http.port = p} mgr x

spec :: Warp.Port -> Spec
spec port = do
  -- tests basic routing
  describe "GET /basic" $ do
    it "responds with 200" $ do
      req port $ choose basic $ GetResponse $ \resp _txt ->
        Http.responseStatus resp `shouldBe` status200

    it "responds with \"basic\"" $ do
      req port $ choose basic $ GetResponse $ \_resp decodeTxt -> do
        Right txt <- return decodeTxt
        txt `shouldBe` "basic"

  -- tests ReqBodyJSON and JSON
  describe "POST /echo-json" $ do
    it "responds with 200" $
      req port $ choose echoJSON $ WithData (String "test") $ GetResponse $ \resp _txt ->
        Http.responseStatus resp `shouldBe` status200

    it "responds with same JSON" $ property $ \val -> do
      ioProperty $ req port $ choose echoJSON $ WithData val $ GetResponse $ \_resp decodeVal -> do
        Right val' <- return decodeVal
        return (val == val')

  -- tests exception handling
  describe "GET /fubar" $ do
    it "responds with 500" $ do
      req port $ choose internalError $ GetResponse $ \resp _ ->
        Http.responseStatus resp `shouldBe` status500

  -- tests Capture
  describe "GET /echo-capture" $ do
    it "responds with 200" $
      req port $ choose echoCapture $ WithData "test" $ GetResponse $ \resp _ ->
        Http.responseStatus resp `shouldBe` status200

    it "responds with captured segment" $ property $ \seg -> do
      ioProperty $ req port $ choose echoCapture $ WithData seg $ GetResponse $ \_ decodeSeg -> do
        Right seg' <- return decodeSeg
        return (seg == seg')

instance Arbitrary Value where
  arbitrary = sized arbJSON
   where
    arbJSON :: Int -> Gen Value
    arbJSON n
      | n == 0 = oneof leaves
      | otherwise = oneof (leaves ++ branches (arbJSON (n `div` 4)))
    leaves =
      [ String <$> arbitrary
      , Number <$> arbitrary
      , Bool <$> arbitrary
      , pure Null
      ]
    branches child =
      [ do
          values <- scale (`div` 4) (listOf child)
          entries <- for values $ \val -> do
            key <- arbitrary
            return ( key, val )
          return $ Object $ HMS.fromList entries
      , (Array . V.fromList) <$> scale (`div` 4) (listOf child)
      ]
  shrink = genericShrink

instance (Eq key, Hashable key, Arbitrary key, Arbitrary value) => Arbitrary (HMS.HashMap key value) where
  arbitrary = HMS.fromList <$> arbitrary
  shrink = map HMS.fromList . shrink . HMS.toList

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary
  shrink = map T.pack . shrink . T.unpack

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary
  shrink = map V.fromList . shrink . V.toList

instance Arbitrary S.Scientific where
  arbitrary = S.scientific <$> arbitrary <*> arbitrary
  shrink s = map (uncurry S.scientific) $ shrink $ ( S.coefficient s, S.base10Exponent s )

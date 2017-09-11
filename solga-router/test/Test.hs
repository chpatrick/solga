{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Test.Hspec.Wai.QuickCheck
import           Test.QuickCheck (genericShrink)

import           Data.Aeson hiding (json)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB
import           Data.Hashable
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HMS
import qualified Data.Vector as V
import           Data.Traversable
import           GHC.Generics (Generic)
import           Network.HTTP.Types.URI
import           Network.Wai.Test

import           Solga.Core
import           Solga.Router

main :: IO ()
main = hspec spec

data TestAPI = TestAPI
  { basic :: "basic" /> Get T.Text
  , echoJSON :: "echo-json" /> ReqBodyJSON Value :> Post Value
  , internalError :: "fubar" /> Get T.Text
  , echoCapture :: "echo-capture" /> Capture T.Text :> Get T.Text
  } deriving (Generic)
instance Router TestAPI

testAPI :: TestAPI
testAPI = TestAPI
  { basic = brief (return "basic")
  , echoJSON = brief return
  , internalError = brief (return $ error "quality programming")
  , echoCapture = brief return
  }

spec :: Spec
spec = with (return $ serve testAPI) $ do
  -- tests basic routing
  describe "GET /basic" $ do
    it "responds with 200" $ do
      get "/basic" `shouldRespondWith` 200

    it "responds with 404 for the wrong method" $ do
      post "/basic" "" `shouldRespondWith` 404

    it "responds with \"basic\"" $ do
      get "/basic" `shouldRespondWith` [json|"basic"|]

  describe "GET /doesnt-exist" $
    it "responds with 404" $ do
      get "/doesnt-exist" `shouldRespondWith` 404

  -- tests ReqBodyJSON and JSON
  describe "POST /echo-json" $ do
    it "responds with 200" $
      post "/echo-json" [json|"test"|] `shouldRespondWith` 200

    it "responds with correct content type" $
      post "/echo-json" [json|"test"|] `shouldRespondWith`
        200 { matchHeaders = ["Content-Type" <:> "application/json"] }

    it "responds with same JSON" $ property $ \val -> do
      resp <- post "/echo-json" (encode val)
      liftIO $ decode (simpleBody resp) `shouldBe` Just (val :: Value)

  -- tests exception handling
  describe "GET /fubar" $ do
    it "responds with 500" $
      get "/fubar" `shouldRespondWith` 500

  -- tests Capture
  describe "GET /echo-capture" $ do
    it "responds with 200" $
      get "/echo-capture/test" `shouldRespondWith` 200

    it "responds with captured segment" $ property $ \seg -> do
      let path = LBS.toStrict $ BSB.toLazyByteString $ encodePathSegments [ "echo-capture", seg ]
      resp <- get path
      liftIO $ decode (simpleBody resp) `shouldBe` Just (String seg)

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

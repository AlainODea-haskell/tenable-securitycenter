-- |
-- Module      : Spec
-- Copyright   : (c) 2016 Alain O'Dea
-- License     : Apache Public License, v. 2.0.
-- Maintainer  : Alain O'Dea <alain.odea@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Tenable SecurityCenter REST API - Tests
--
-- Smoke tests for REST API client

{-# LANGUAGE OverloadedStrings #-}
import Network.Tenable.SecurityCenter.Client
import Network.Tenable.SecurityCenter.Token
import Network.Tenable.SecurityCenter.Types
import Network.Tenable.SecurityCenter.Asset

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Token response" $ do
    it "create token response parses correctly" $ do
      decode tokenResponseRaw `shouldBe` tokenResponseParsed
  describe "Asset response" $ do
    it "get asset by ID response parses correctly" $ do
      decode staticAssetResponseRaw `shouldBe` staticAssetResponseParsed
  describe "Asset requests" $
    it "update defined IPs request builds correctly" $ do
      encode updateDefinedIPsRequestData `shouldBe` updateDefinedIPsRequestWire

tokenResponseRaw :: L8.ByteString
tokenResponseRaw = "{\"type\":\"regular\",\"response\":{\"token\":123456789,\"unassociatedCert\":\"false\"},\"error_code\":0,\"error_msg\":\"\",\"warnings\":[],\"timestamp\":1403115433}\n"

tokenResponseParsed :: Maybe (ApiResponse CreateTokenResponse)
tokenResponseParsed = Just (ApiResponse {apiResponseResponse = CreateTokenResponse {createTokenResponseToken = Token 123456789}})

staticAssetResponseRaw :: L8.ByteString
staticAssetResponseRaw  = "{\"type\":\"regular\",\"response\":{\"id\":\"1234\",\"name\":\"Private Networks\",\"type\":\"static\",\"description\":\"\",\"tags\":\"\",\"context\":\"\",\"status\":\"0\",\"createdTime\":\"1464202127\",\"modifiedTime\":\"1464202127\",\"typeFields\":{\"definedIPs\":\"10.0.0.0/8,172.16.0.0/12,192.168.0.0/16\"},\"repositories\":[{\"ipCount\":\"0\",\"repository\":{\"id\":\"1\",\"name\":\"staging-creds\",\"description\":\"\"}},{\"ipCount\":\"0\",\"repository\":{\"id\":\"2\",\"name\":\"development-creds\",\"description\":\"\"}},{\"ipCount\":\"0\",\"repository\":{\"id\":\"3\",\"name\":\"production-creds\",\"description\":\"\"}}],\"ipCount\":\"17891328\",\"groups\":[],\"assetDataFields\":[],\"canUse\":\"true\",\"canManage\":\"true\",\"creator\":{\"id\":\"1\",\"username\":\"alain.odea\",\"firstname\":\"Alain\",\"lastname\":\"O'Dea\"},\"owner\":{\"id\":\"1\",\"username\":\"alain.odea\",\"firstname\":\"Alain\",\"lastname\":\"O'Dea\"},\"ownerGroup\":{\"id\":\"0\",\"name\":\"Full Access\",\"description\":\"Full Access group\"},\"targetGroup\":{\"id\":-1,\"name\":\"\",\"description\":\"\"},\"template\":{\"id\":-1,\"name\":\"\",\"description\":\"\"}},\"error_code\":0,\"error_msg\":\"\",\"warnings\":[],\"timestamp\":1464109434}\n"

staticAssetResponseParsed :: Maybe (ApiResponse GetAssetByIdResponse)
staticAssetResponseParsed = Just (ApiResponse {apiResponseResponse = GetAssetByIdResponse {assetByIdId = "1234", assetByIdStaticTypeFields = StaticTypeFields {typeFieldsDefinedIPs = "10.0.0.0/8,172.16.0.0/12,192.168.0.0/16"}}})

updateDefinedIPsRequestData :: UpdateDefinedIPsRequest
updateDefinedIPsRequestData = UpdateDefinedIPsRequest
                              { updateDefinedIPsAssetId = "1234"
                              , updateDefinedIPsDefinedIPs =
                                [ "10.0.0.0/8"
                                , "172.16.0.0/12"
                                , "192.168.0.0/16"
                                ]
                              }

updateDefinedIPsRequestWire :: L8.ByteString
updateDefinedIPsRequestWire = "{\"definedIPs\":\"10.0.0.0/8,172.16.0.0/12,192.168.0.0/16\"}"

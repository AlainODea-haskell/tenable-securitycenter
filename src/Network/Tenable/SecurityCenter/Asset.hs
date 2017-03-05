-- |
-- Module      : Network.Tenable.SecurityCenter.Token
-- Copyright   : (c) 2016 Alain O'Dea
-- License     : Apache Public License, v. 2.0.
-- Maintainer  : Alain O'Dea <alain.odea@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Tenable SecurityCenter REST API - Asset Requests
--
-- The Asset endpoint is used to manage Assets (lists of IPs and hosts)
-- in SecurityCenter.

{-# LANGUAGE OverloadedStrings #-}
module Network.Tenable.SecurityCenter.Asset where

import Network.Tenable.SecurityCenter.Types (Endpoint(..))

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8

data ListAssetsRequest = ListAssetsRequest
                         { authenticationToken :: S8.ByteString
                         } deriving Show

instance Endpoint ListAssetsRequest where
  endpointRequestMethod _ = "GET"
  endpointRequestPath _ = "/rest/asset"
  endpointAuthentication = pure . authenticationToken

instance ToJSON ListAssetsRequest where
  toJSON _ = Null

data ListAssetsResponse = ListAssetsResponse
                          { usableAssets :: [ListAssetResponse]
                          , manageableAssets :: [ListAssetResponse]
                          } deriving Show

instance FromJSON ListAssetsResponse where
  parseJSON (Object v) = ListAssetsResponse <$>
                         v .: "usable" <*>
                         v .: "manageable"
  parseJSON invalid = typeMismatch "ListAssetsResponse" invalid

data ListAssetResponse = ListAssetResponse
                         { assetId :: T.Text
                         , assetName :: T.Text
                         , assetDescription :: T.Text
                         , assetStatus :: T.Text
                         } deriving Show

instance FromJSON ListAssetResponse where
  parseJSON (Object v) = ListAssetResponse <$>
                         v .: "id" <*>
                         v .: "name" <*>
                         v .: "description" <*>
                         v .: "status"
  parseJSON invalid = typeMismatch "ListAssetResponse" invalid

data GetAssetByIdRequest = GetAssetByIdRequest
                           { getAssetByIdToken :: S8.ByteString
                           , getAssetByIdId :: S8.ByteString
                           } deriving Show

instance Endpoint GetAssetByIdRequest where
  endpointRequestMethod _ = "GET"
  endpointRequestPath x = S8.concat ["/rest/asset/", getAssetByIdId x]
  endpointRequestQueryString _ =
    [ ("fields", pure "id,typeFields")
    ]
  endpointAuthentication = pure . getAssetByIdToken

instance ToJSON GetAssetByIdRequest where
  toJSON _ = Null

data GetAssetByIdResponse = GetAssetByIdResponse
                            { assetByIdId :: T.Text
                            , assetByIdStaticTypeFields :: StaticTypeFields
                            } deriving (Eq, Show)

instance FromJSON GetAssetByIdResponse where
  parseJSON (Object v) =
     GetAssetByIdResponse <$>
                          v .: "id" <*>
                          v .: "typeFields"
  parseJSON invalid = typeMismatch "GetAssetByIdResponse" invalid

data StaticTypeFields = StaticTypeFields
                        { typeFieldsDefinedIPs :: T.Text
                        } deriving (Eq, Show)

instance FromJSON StaticTypeFields where
  parseJSON (Object v) = StaticTypeFields <$>
                         v .: "definedIPs"
  parseJSON invalid = typeMismatch "StaticTypeFields" invalid

data UpdateDefinedIPsRequest = UpdateDefinedIPsRequest
                              { updateDefinedIPsAssetId :: S8.ByteString
                              , updateDefinedIPsDefinedIPs :: [T.Text]
                              , updateDefinedIPsToken :: S8.ByteString
                              } deriving Show

instance Endpoint UpdateDefinedIPsRequest where
  endpointRequestMethod _ = "PATCH"
  endpointRequestPath x = S8.concat ["/rest/asset/", updateDefinedIPsAssetId x]
  endpointAuthentication = pure . updateDefinedIPsToken

instance ToJSON UpdateDefinedIPsRequest where
  toJSON x = object ["definedIPs" .= (T.intercalate "," $
                                      updateDefinedIPsDefinedIPs x)]

data CreateStaticAssetRequest = CreateStaticAssetRequest
                                { createStaticAssetRequestName :: T.Text
                                , createStaticAssetRequestDefinedIPs :: [T.Text]
                                , createStaticAssetRequestToken :: S8.ByteString
                                }

instance Endpoint CreateStaticAssetRequest where
  endpointRequestMethod _ = "POST"
  endpointRequestPath _ = "/rest/asset"
  endpointAuthentication = pure . createStaticAssetRequestToken

instance ToJSON CreateStaticAssetRequest where
  toJSON x = object
    [ "name" .= createStaticAssetRequestName x
    , "type" .= ("static" :: T.Text)
    , "definedIPs" .= (T.intercalate "," $
                     createStaticAssetRequestDefinedIPs x)]

-- |
-- Module      : Network.Tenable.SecurityCenter.Asset
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
module Network.Tenable.SecurityCenter.Asset
       ( ListAssetsRequest(..)
       , ListAssetsResponse(..)
       , GetAssetByIdRequest(..)
       , GetAssetByIdResponse(..)
       , StaticTypeFields(..)
       , UpdateDefinedIPsRequest(..)
       , CreateStaticAssetRequest(..)
       )
where

import Network.Tenable.SecurityCenter.Types (Endpoint(..))

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text as T

{-| 'ListAssetsRequest' represents a request for a list of assets.
-}
data ListAssetsRequest = ListAssetsRequest
                         deriving Show

instance Endpoint ListAssetsRequest where
  endpointRequestMethod _ = "GET"
  endpointRequestPath _ = "/rest/asset"

instance ToJSON ListAssetsRequest where
  toJSON _ = Null

{-| 'ListAssetsResponse' represents a list of assets.
-}
data ListAssetsResponse = ListAssetsResponse
                          { usableAssets :: [ListAssetResponse]
                          -- ^ Assets visible to the authenticated user
                          , manageableAssets :: [ListAssetResponse]
                          -- ^ Assets editable by the authenticated user
                          } deriving Show

instance FromJSON ListAssetsResponse where
  parseJSON (Object v) = ListAssetsResponse <$>
                         v .: "usable" <*>
                         v .: "manageable"
  parseJSON invalid = typeMismatch "ListAssetsResponse" invalid

{-| 'ListAssetResponse' represents an individual asset.
-}
data ListAssetResponse = ListAssetResponse
                         { assetId :: T.Text
                         -- ^ Unique ID of asset in SecurityCenter
                         , assetName :: T.Text
                         -- ^ Display name of asset
                         , assetDescription :: T.Text
                         -- ^ Description of asset
                         , assetStatus :: T.Text
                         -- ^ Status of asset
                         } deriving Show

instance FromJSON ListAssetResponse where
  parseJSON (Object v) = ListAssetResponse <$>
                         v .: "id" <*>
                         v .: "name" <*>
                         v .: "description" <*>
                         v .: "status"
  parseJSON invalid = typeMismatch "ListAssetResponse" invalid

{-| 'GetAssetByIdRequest' represents a request to get details of an
    asset.
-}
data GetAssetByIdRequest = GetAssetByIdRequest
                           { getAssetByIdId :: T.Text
                           -- ^ Unique ID of asset to retrieve
                           } deriving Show

instance Endpoint GetAssetByIdRequest where
  endpointRequestMethod _ = "GET"
  endpointRequestPath x = T.concat ["/rest/asset/", getAssetByIdId x]
  endpointRequestQueryString _ =
    [ ("fields", pure "id,typeFields")
    ]

instance ToJSON GetAssetByIdRequest where
  toJSON _ = Null

{-| 'GetAssetByIdResponse' represents the details of an individual
    asset.
-}
data GetAssetByIdResponse = GetAssetByIdResponse
                            { assetByIdId :: T.Text
                            -- ^ Unique ID of asset retrieved
                            , assetByIdStaticTypeFields :: StaticTypeFields
                            -- ^ Type fields (really just a wrapped for definedIPs)
                            } deriving (Eq, Show)

instance FromJSON GetAssetByIdResponse where
  parseJSON (Object v) =
     GetAssetByIdResponse <$>
                          v .: "id" <*>
                          v .: "typeFields"
  parseJSON invalid = typeMismatch "GetAssetByIdResponse" invalid

{-| 'StaticTypeFields' represents a request the typeFields of a static
    type asset.
-}
data StaticTypeFields = StaticTypeFields
                        { typeFieldsDefinedIPs :: T.Text
                        -- ^ IPs/IP ranges on the asset separated by commas or spaces (TODO: should really be parsed into a list)
                        } deriving (Eq, Show)

instance FromJSON StaticTypeFields where
  parseJSON (Object v) = StaticTypeFields <$>
                         v .: "definedIPs"
  parseJSON invalid = typeMismatch "StaticTypeFields" invalid

{-| 'UpdateDefinedIPsRequest' represents a request to update the IPs
    listed on a static asset.
-}
data UpdateDefinedIPsRequest = UpdateDefinedIPsRequest
                              { updateDefinedIPsAssetId :: T.Text
                              -- ^ Unique ID of an existing static asset to update
                              , updateDefinedIPsDefinedIPs :: [T.Text]
                              -- ^ New list of IPs/CIDRs for the asset (SecurityCenter will convert into ranges)
                              } deriving Show

instance Endpoint UpdateDefinedIPsRequest where
  endpointRequestMethod _ = "PATCH"
  endpointRequestPath x = T.concat
    [ "/rest/asset/" , updateDefinedIPsAssetId x]

instance ToJSON UpdateDefinedIPsRequest where
  toJSON x = object ["definedIPs" .= (T.intercalate "," $
                                      updateDefinedIPsDefinedIPs x)]

{-| 'CreateStaticAssetRequest' represents a request to create an asset.
-}
data CreateStaticAssetRequest = CreateStaticAssetRequest
                                { createStaticAssetRequestName :: T.Text
                                -- ^ Display name for the asset
                                , createStaticAssetRequestDefinedIPs :: [T.Text]
                                -- ^ Initial set of IPs/CIDRs for the asset (SecurityCenter will convert into ranges)
                                }

instance Endpoint CreateStaticAssetRequest where
  endpointRequestMethod _ = "POST"
  endpointRequestPath _ = "/rest/asset"

instance ToJSON CreateStaticAssetRequest where
  toJSON x = object
    [ "name" .= createStaticAssetRequestName x
    , "type" .= ("static" :: T.Text)
    , "definedIPs" .= (T.intercalate "," $
                     createStaticAssetRequestDefinedIPs x)]

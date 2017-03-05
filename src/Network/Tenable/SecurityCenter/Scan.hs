-- |
-- Module      : Network.Tenable.SecurityCenter.Scan
-- Copyright   : (c) 2017 Alain O'Dea
-- License     : Apache Public License, v. 2.0.
-- Maintainer  : Alain O'Dea <alain.odea@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Tenable SecurityCenter REST API - Scan Requests
--
-- The Scan endpoint is used to manage vulnerability scan configurations
-- in SecurityCenter.

{-# LANGUAGE OverloadedStrings #-}
module Network.Tenable.SecurityCenter.Scan
       ( ListScansRequest(..)
       , ListScansResponse(..)
       )
where

import Network.Tenable.SecurityCenter.Types (Endpoint(..))

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text as T

{-| 'ListScansRequest' represents a request for a list of scans.
-}
data ListScansRequest = ListScansRequest
                        deriving Show

instance Endpoint ListScansRequest where
  endpointRequestMethod _ = "GET"
  endpointRequestPath _ = "/rest/scan"

instance ToJSON ListScansRequest where
  toJSON _ = Null

{-| 'ListScansResponse' represents a list of scans.
-}
data ListScansResponse = ListScansResponse
                          { scanUsable :: [ListScanResponse]
                          -- ^ Scans visible to the authenticated user
                          , scanManageable :: [ListScanResponse]
                          -- ^ Scans editable by the authenticated user
                          } deriving Show

instance FromJSON ListScansResponse where
  parseJSON (Object v) = ListScansResponse <$>
                         v .: "usable" <*>
                         v .: "manageable"
  parseJSON invalid = typeMismatch "ListScansResponse" invalid

{-| 'ListScanResponse' represents an individual scan.
-}
data ListScanResponse = ListScanResponse
                          { scanId :: T.Text
                          -- ^ Unique ID of scan in SecurityCenter
                          , scanName :: T.Text
                          -- ^ Short name for scan in lists
                          , scanDescription :: T.Text
                          -- ^ Detailed decription of scan configuration
                          } deriving Show

instance FromJSON ListScanResponse where
  parseJSON (Object v) = ListScanResponse <$>
                         v .: "id" <*>
                         v .: "name" <*>
                         v .: "description"
  parseJSON invalid = typeMismatch "ListScanResponse" invalid

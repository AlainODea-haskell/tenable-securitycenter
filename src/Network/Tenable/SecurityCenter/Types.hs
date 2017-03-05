-- |
-- Module      : Network.Tenable.SecurityCenter.Types
-- Copyright   : (c) 2016 Alain O'Dea
-- License     : Apache Public License, v. 2.0.
-- Maintainer  : Alain O'Dea <alain.odea@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Tenable SecurityCenter REST API - Types
--
-- Types, data types, and typeclasses common to all modules.

{-# LANGUAGE OverloadedStrings #-}
module Network.Tenable.SecurityCenter.Types
       ( ApiResponse(..)
       , Endpoint(..)
       , Token(..)
       )
where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text as T

{-| 'ApiResponse' represents the general top-level response wrapper
    SecurityCenter produces in all API responses.
-}
data ApiResponse a = ApiResponse
                     { apiResponseResponse :: a
                     -- ^ Endpoint-specific response
                     } deriving (Eq, Show)

instance (FromJSON a) => FromJSON (ApiResponse a) where
  parseJSON (Object v) = ApiResponse <$>
                         v .: "response"
  parseJSON invalid = typeMismatch "ApiResponse" invalid

{-| 'Endpoint' describes the relationship of a request representation to
    the location and interface of the RESTful resource the SecurityCenter
    server exposes.
-}
class Endpoint a where
  -- | HTTP request method (GET|POST|PUT|DELETE or offbeat ones like PATCH)
  endpointRequestMethod :: a -> T.Text
  -- | Request path (example \"/rest/token\")
  endpointRequestPath :: a -> T.Text
  -- | URL Query parameters (if needed, defaults to [])
  endpointRequestQueryString :: a -> [(T.Text, Maybe T.Text)]
  endpointRequestQueryString = const []

{-| 'Token' represents a SecurityCenter authentication token
-}
newtype Token = Token { unToken :: Int }
  deriving (Eq, Show)

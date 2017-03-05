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
module Network.Tenable.SecurityCenter.Types where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8

data ApiResponse a = ApiResponse
                     { apiType :: T.Text
                     , response :: a
                     , errorCode :: Int
                     , errorMsg :: T.Text
                     , warnings :: [T.Text]
                     , timestamp :: Int
                     } deriving (Eq, Show)

instance (FromJSON a) => FromJSON (ApiResponse a) where
  parseJSON (Object v) = ApiResponse <$>
                         v .: "type" <*>
                         v .: "response" <*>
                         v .: "error_code" <*>
                         v .: "error_msg" <*>
                         v .: "warnings" <*>
                         v .: "timestamp"
  parseJSON invalid = typeMismatch "ApiResponse" invalid

class Endpoint a where
  endpointRequestMethod :: a -> S8.ByteString
  endpointRequestPath :: a -> S8.ByteString
  endpointRequestQueryString :: a -> [(S8.ByteString, Maybe S8.ByteString)]
  endpointRequestQueryString = const []
  endpointAuthentication :: a -> Maybe S8.ByteString

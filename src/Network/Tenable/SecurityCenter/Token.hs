-- |
-- Module      : Network.Tenable.SecurityCenter.Token
-- Copyright   : (c) 2016 Alain O'Dea
-- License     : Apache Public License, v. 2.0.
-- Maintainer  : Alain O'Dea <alain.odea@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Tenable SecurityCenter REST API - Token Endpoint Client
--
-- The Token endpoint is used to log into the SecurityCenter REST API
-- and get a session token to be used with other requests.

{-# LANGUAGE OverloadedStrings #-}
module Network.Tenable.SecurityCenter.Token
       ( CreateTokenRequest(..)
       , CreateTokenResponse(..)
       , DeleteTokenRequest(..)
       )
where

import Network.Tenable.SecurityCenter.Types
  ( Endpoint(..)
  , HttpMethod(HttpPost, HttpDelete)
  , Token(..)
  )

import Control.Applicative (liftA)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T

{-| 'CreateTokenRequest' represents a request to log in.
-}
data CreateTokenRequest = CreateTokenRequest
                          { createTokenRequestUsername :: T.Text
                          -- ^ API user's username
                          , createTokenRequestPassword :: T.Text
                          -- ^ API user's password
                          } deriving Show

instance Endpoint CreateTokenRequest where
  endpointRequestMethod _ = HttpPost
  endpointRequestPath _ = "/rest/token"

instance ToJSON CreateTokenRequest where
  toJSON tokenRequest =
    object [ "username" .= createTokenRequestUsername tokenRequest
           , "password" .= createTokenRequestPassword tokenRequest]

{-| 'CreateTokenResponse' represents a successful login and includes the
    authentication token needed for subsequent requests.
-}
data CreateTokenResponse = CreateTokenResponse
                           { createTokenResponseToken :: Token
                           -- ^ Authentication token to be used in subsequent requests.
                           } deriving (Eq, Show)

instance FromJSON CreateTokenResponse where
  parseJSON (Object v) = liftA CreateTokenResponse
                         (Token <$> (v .: "token"))
  parseJSON invalid = typeMismatch "CreateTokenResponse" invalid

{-| 'DeleteTokenRequest' represents a request to log out.
-}
data DeleteTokenRequest = DeleteTokenRequest
                          deriving Show

instance Endpoint DeleteTokenRequest where
  endpointRequestMethod _ = HttpDelete
  endpointRequestPath _ = "/rest/token"

instance ToJSON DeleteTokenRequest where
  toJSON _ = Null

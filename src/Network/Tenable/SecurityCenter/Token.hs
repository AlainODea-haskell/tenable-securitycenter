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
module Network.Tenable.SecurityCenter.Token where

import Network.Tenable.SecurityCenter.Types (Endpoint(..), Token(..))

import Control.Applicative (liftA)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T

data CreateTokenRequest = CreateTokenRequest
                          { username :: T.Text
                          , password :: T.Text
                          } deriving Show

instance Endpoint CreateTokenRequest where
  endpointRequestMethod _ = "POST"
  endpointRequestPath _ = "/rest/token"
  endpointAuthentication _ = Nothing

instance ToJSON CreateTokenRequest where
  toJSON tokenRequest =
    object [ "username" .= username tokenRequest
           , "password" .= password tokenRequest]

data CreateTokenResponse = CreateTokenResponse
                           { token :: Token
                           } deriving (Eq, Show)

instance FromJSON CreateTokenResponse where
  parseJSON (Object v) = liftA CreateTokenResponse
                         (Token <$> (v .: "token"))
  parseJSON invalid = typeMismatch "CreateTokenResponse" invalid

data DeleteTokenRequest = DeleteTokenRequest
                          { deleteTokenRequestToken :: Token
                          } deriving Show

instance Endpoint DeleteTokenRequest where
  endpointRequestMethod _ = "DELETE"
  endpointRequestPath _ = "/rest/token"
  endpointAuthentication x = Just $ deleteTokenRequestToken x

instance ToJSON DeleteTokenRequest where
  toJSON _ = Null

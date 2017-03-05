-- |
-- Module      : Network.Tenable.SecurityCenter.Token
-- Copyright   : (c) 2016 Alain O'Dea
-- License     : Apache Public License, v. 2.0.
-- Maintainer  : Alain O'Dea <alain.odea@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Tenable SecurityCenter REST API - Client
--
-- Handles request execution, sessions, and authentication

{-# LANGUAGE OverloadedStrings #-}
module Network.Tenable.SecurityCenter.Client where

import Network.Tenable.SecurityCenter.Types

import           Data.Aeson
import           Data.Bool (bool)
import qualified Data.ByteString.Char8 as S8
import           Network.Connection (TLSSettings(..))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.HTTP.Conduit
import           Network.HTTP.Simple

runRequest :: (Endpoint a, ToJSON a, FromJSON b)
           => S8.ByteString
           -> a
           -> CookieJar
           -> IO ((Maybe (ApiResponse b)), CookieJar)
runRequest hostname apiRequest session = do
  manager <- newManager noVerifyTlsManagerSettings
  let request
        = setApiRequestBody apiRequest
        $ defaultApiRequest hostname apiRequest manager session
  apiResponse <- httpLBS request
  let apiResponseBody = getResponseBody apiResponse
  return $ (decode apiResponseBody, responseCookieJar apiResponse)

setApiRequestBody :: (Endpoint a, ToJSON a)
                  => a
                  -> Request
                  -> Request
setApiRequestBody apiRequest req =
  bool req (setRequestBodyJSON apiRequest req)
  $ endpointRequestMethod apiRequest `elem` ["POST","PATCH"]

defaultApiRequest :: (Endpoint a, ToJSON a)
               => S8.ByteString
               -> a
               -> Manager
               -> CookieJar
               -> Request
defaultApiRequest hostname apiRequest manager session =
  setRequestMethod (endpointRequestMethod apiRequest)
  $ setRequestPath (endpointRequestPath apiRequest)
  $ setRequestQueryString (endpointRequestQueryString apiRequest)
  $ setRequestAuthentication apiRequest
  $ setRequestManager manager
  $ setRequestHost hostname
  $ setRequestSecure True
  $ setRequestPort 443
  $ defaultRequest {
    cookieJar = Just session
  }

setRequestAuthentication :: Endpoint a
                         => a
                         -> Request
                         -> Request
setRequestAuthentication apiRequest req =
  maybe req
  (flip (setRequestHeader "X-SecurityCenter") req)
  $ fmap (:[]) $ endpointAuthentication apiRequest

noVerifyTlsManagerSettings :: ManagerSettings
noVerifyTlsManagerSettings = mkManagerSettings noVerifyTlsSettings Nothing

noVerifyTlsSettings :: TLSSettings
noVerifyTlsSettings = TLSSettingsSimple
  { settingDisableCertificateValidation = True
  , settingDisableSession = True
  , settingUseServerName = False
  }

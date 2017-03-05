-- |
-- Module      : Network.Tenable.SecurityCenter.Client
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
module Network.Tenable.SecurityCenter.Client
       (runRequest)
where

import Network.Tenable.SecurityCenter.Types

import           Data.Aeson
import           Data.Bool (bool)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Network.HTTP.Client
                 ( cookieJar
                 , responseCookieJar
                 , Manager
                 , CookieJar
                 )
import           Network.HTTP.Simple

-- | Run an API request and get a response
runRequest :: (Endpoint a, ToJSON a, FromJSON b)
           => Manager
           -> T.Text
           -> CookieJar
           -> a
           -> Maybe Token
           -> IO (Maybe b, CookieJar)
runRequest manager hostname session apiRequest token = do
  let request
        = setApiRequestBody apiRequest
        $ defaultApiRequest hostname apiRequest token manager session
  apiResponse <- httpLBS request
  let apiResponseBody = getResponseBody apiResponse
  return $ ( fmap apiResponseResponse $ decode apiResponseBody
           , responseCookieJar apiResponse
           )

setApiRequestBody :: (Endpoint a, ToJSON a)
                  => a
                  -> Request
                  -> Request
setApiRequestBody apiRequest req =
  bool req (setRequestBodyJSON apiRequest req)
  $ endpointRequestMethod apiRequest `elem` [HttpPost, HttpPatch]

defaultApiRequest :: (Endpoint a, ToJSON a)
               => T.Text
               -> a
               -> Maybe Token
               -> Manager
               -> CookieJar
               -> Request
defaultApiRequest hostname apiRequest token manager session =
  setRequestMethod (TE.encodeUtf8 $ httpMethod $ endpointRequestMethod apiRequest)
  $ setRequestPath (TE.encodeUtf8 $ endpointRequestPath apiRequest)
  $ setRequestQueryString (fmap encodeQuery $ endpointRequestQueryString apiRequest)
  $ setRequestAuthentication token
  $ setRequestManager manager
  $ setRequestHost (TE.encodeUtf8 $ hostname)
  $ setRequestSecure True
  $ setRequestPort 443
  $ defaultRequest {
    cookieJar = Just session
  }

encodeQuery :: (T.Text, Maybe T.Text)
            -> (S8.ByteString, Maybe S8.ByteString)
encodeQuery (k, v) = (TE.encodeUtf8 k, fmap TE.encodeUtf8 v)

setRequestAuthentication :: Maybe Token
                         -> Request
                         -> Request
setRequestAuthentication token req =
  maybe req
  (flip (setRequestHeader "X-SecurityCenter") req)
  $ fmap ((:[]) . S8.pack . show . unToken) token

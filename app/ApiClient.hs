-- |
-- Module      : ApiClient
-- Copyright   : (c) 2017 Alain O'Dea
-- License     : Apache Public License, v. 2.0.
-- Maintainer  : Alain O'Dea <alain.odea@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Tenable SecurityCenter REST API - API Client for examples
--
-- Configuration parsing and API Client for running examples.

{-# LANGUAGE OverloadedStrings #-}
module ApiClient where

import Network.Tenable.SecurityCenter.Client (runRequest)
import Network.Tenable.SecurityCenter.Token
  ( CreateTokenRequest(..)
  , CreateTokenResponse(..)
  , DeleteTokenRequest(..)
  )
import Network.Tenable.SecurityCenter.Types
  ( Token
  , ApiResponse
  , Endpoint(..)
  )

import           Data.Aeson (eitherDecode)
import           Data.Aeson.Types
  ( FromJSON(parseJSON)
  , ToJSON
  , Value(Object)
  , typeMismatch
  , (.:)
  )
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Network.HTTP.Conduit
  ( Manager
  , CookieJar
  , createCookieJar
  , newManager
  , tlsManagerSettings
  )
import qualified Data.Time.Clock as Clock
import           Data.Time.ISO8601 (formatISO8601Millis)
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
                         v .: "host" <*>
                         v .: "username" <*>
                         v .: "password"
  parseJSON invalid = typeMismatch "Config" invalid

data Config = Config
              { securityCenterHost :: T.Text
              , securityCenterUsername :: T.Text
              , securityCenterPassword :: T.Text
              }

data ApiClient = ApiClient
                 { apiClientManager :: Manager
                 , apiClientHostname :: T.Text
                 , apiClientSession :: CookieJar
                 , apiClientToken :: Token
                 }

createApiClient :: FilePath
                -> IO ApiClient
createApiClient configFilename = do
  configFile <- L8.readFile configFilename
  let config = either error id $ eitherDecode configFile
  let hostname = securityCenterHost $ config
  let u = securityCenterUsername config
  let p = securityCenterPassword config
  manager <- newManager tlsManagerSettings
  (t, session) <- getToken manager hostname u p
  return $ ApiClient manager hostname session t

getToken :: Manager
         -> T.Text
         -> T.Text
         -> T.Text
         -> IO (Token, CookieJar)
getToken manager hostname u p = do
  let unauthSession = createCookieJar []
  let req = CreateTokenRequest u p
  (res, authSession) <- runRequest manager hostname unauthSession req Nothing
  let Just t = fmap createTokenResponseToken res
  return (t, authSession)

runApiRequest :: (Endpoint a, ToJSON a, FromJSON b)
              => ApiClient
              -> a
              -> IO (Maybe b, CookieJar)
runApiRequest apiClient req = do
  reqId <- U.nextRandom
  logSendApi reqId req
  let manager = apiClientManager apiClient
  let hostname = apiClientHostname apiClient
  let session = apiClientSession apiClient
  res <- runRequest manager hostname session req $
    Just $ apiClientToken apiClient
  logSuccessApi reqId req
  return res

logSendApi :: Endpoint a
           => U.UUID
           -> a
           -> IO ()
logSendApi = logApi "Sending..."

logSuccessApi :: Endpoint a
              => U.UUID
              -> a
              -> IO ()
logSuccessApi = logApi "Success"

logApi :: Endpoint a
           => T.Text
           -> U.UUID
           -> a
           -> IO ()
logApi msg reqId req = do
  currentTime <- Clock.getCurrentTime
  T.putStrLn $ T.concat
    [ T.pack $ formatISO8601Millis currentTime
    , ":"
    , U.toText reqId
    , ":"
    , msg
    , ":"
    , endpointRequestMethod req
    , ":"
    , endpointRequestPath req
    ]

endSession :: ApiClient
           -> IO (Maybe (ApiResponse Value), CookieJar)
endSession apiClient = do
  let req = DeleteTokenRequest
  runApiRequest apiClient req

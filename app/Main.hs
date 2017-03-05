-- |
-- Module      : Main
-- Copyright   : (c) 2016 Alain O'Dea
-- License     : Apache Public License, v. 2.0.
-- Maintainer  : Alain O'Dea <alain.odea@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Tenable SecurityCenter REST API - Usage Examples
--
-- Some simple examples of using the REST API

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Tenable.SecurityCenter.Client
import Network.Tenable.SecurityCenter.Token
import Network.Tenable.SecurityCenter.Types
import Network.Tenable.SecurityCenter.Asset
import Network.Tenable.SecurityCenter.Scan

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Either (either)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import           Data.Time.ISO8601 (formatISO8601Millis)
import qualified Data.Time.Clock as Clock
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Conduit
import           System.Environment (getArgs)
import           System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  (configFilename, assetIdArg) <- parseArgs
  apiClient <- createApiClient configFilename
  run_example (example_updateAsset assetIdArg) apiClient

type TSCHost = T.Text
type TSCUser = T.Text
type TSCPass = T.Text

createApiClient :: FilePath
                -> IO ApiClient
createApiClient configFilename = do
  (hostname, u, p) <- readConfig configFilename
  manager <- newManager tlsManagerSettings
  (t, session) <- getToken manager hostname u p
  return $ ApiClient manager hostname session t

readConfig :: FilePath
           -> IO (TSCHost, TSCUser, TSCPass)
readConfig configFilename = do
  configFile <- L8.readFile configFilename
  let config = either error id $ eitherDecode configFile
  let hostname = securityCenterHost $ config
  let u = securityCenterUsername config
  let p = securityCenterPassword config
  return (hostname, u, p)

run_example :: (ApiClient -> IO ())
            -> ApiClient
            -> IO ()
run_example example apiClient = do
  example apiClient
  _ <- endSession apiClient
  return ()

parseArgs :: IO (FilePath, T.Text)
parseArgs = do
  [configFilename, assetIdArg] <- getArgs
  return (configFilename, T.pack assetIdArg)

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

data ApiClient = ApiClient
                 { apiClientManager :: Manager
                 , apiClientHostname :: T.Text
                 , apiClientSession :: CookieJar
                 , apiClientToken :: Token
                 }

example_updateAsset :: T.Text
                    -> ApiClient
                    -> IO ()
example_updateAsset assetToUpdate apiClient = do
  rawUpdate <- T.getContents
  let definedIPs = T.lines rawUpdate
  _ <- updateDefinedIPs apiClient assetToUpdate definedIPs
  return ()

example_createAsset :: T.Text
                    -> ApiClient
                    -> IO ()
example_createAsset desiredAssetName apiClient = do
  rawUpdate <- T.getContents
  let definedIPs = T.lines rawUpdate
  res <- createDefinedIPs apiClient desiredAssetName definedIPs
  let Just createdAssetId = fmap assetByIdId res
  T.putStrLn createdAssetId

data Config = Config
              { securityCenterHost :: T.Text
              , securityCenterUsername :: T.Text
              , securityCenterPassword :: T.Text
              }

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
                         v .: "host" <*>
                         v .: "username" <*>
                         v .: "password"
  parseJSON invalid = typeMismatch "Config" invalid

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

endSession :: ApiClient
           -> IO (Maybe (ApiResponse Object), CookieJar)
endSession apiClient = do
  let req = DeleteTokenRequest
  runApiRequest apiClient req

updateDefinedIPs :: ApiClient
                 -> T.Text
                 -> [T.Text]
                 -> IO (Maybe GetAssetByIdResponse)
updateDefinedIPs apiClient assetToUpdate definedIPs = do
  let req = UpdateDefinedIPsRequest
            { updateDefinedIPsAssetId = assetToUpdate
            , updateDefinedIPsDefinedIPs = definedIPs
            }
  (res, _) <- runApiRequest apiClient req
  return res

createDefinedIPs :: ApiClient
                 -> T.Text
                 -> [T.Text]
                 -> IO (Maybe GetAssetByIdResponse)
createDefinedIPs apiClient assetName definedIPs = do
  let req = CreateStaticAssetRequest
            { createStaticAssetRequestName = assetName
            , createStaticAssetRequestDefinedIPs = definedIPs
            }
  (res, _) <- runApiRequest apiClient req
  return res

listAssets :: ApiClient
           -> Manager
           -> IO ()
listAssets apiClient manager = do
   let req = ListAssetsRequest
   (res, _) <- runApiRequest apiClient req
   let usables = fmap usableAssets res
   print usables
   let manageables = fmap manageableAssets res
   print manageables

getAssetById :: ApiClient
             -> T.Text
             -> IO (Maybe GetAssetByIdResponse)
getAssetById apiClient assetToUpdate = do
  let req = GetAssetByIdRequest
             { getAssetByIdId = assetToUpdate
             }
  (res, _) <- runApiRequest apiClient req
  return res

example_listScans :: ApiClient
          -> IO ()
example_listScans apiClient = do
   let req = ListScansRequest
   (res, _) <- runApiRequest apiClient req
   let usables = fmap scanUsable res
   print usables
   let manageables = fmap scanManageable res
   print manageables

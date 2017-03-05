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

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Either (either)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Conduit
import           System.Environment (getArgs)
import           System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  hSetBuffering stdout NoBuffering
  [configFilename, assetIdArg] <- parseArgs
  configFile <- L8.readFile configFilename
  let config = either error id $ eitherDecode configFile
  let hostname = securityCenterHost $ config
  let u = securityCenterUsername config
  let p = securityCenterPassword config
  (t, session) <- getToken manager hostname u p
  let apiClient = ApiClient manager hostname session t
  example_updateAsset apiClient assetIdArg
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
  let manager = apiClientManager apiClient
  let hostname = apiClientHostname apiClient
  let session = apiClientSession apiClient
  runRequest manager hostname session req

data ApiClient = ApiClient
                 { apiClientManager :: Manager
                 , apiClientHostname :: T.Text
                 , apiClientSession :: CookieJar
                 , apiClientToken :: Token
                 }

example_updateAsset :: ApiClient
                    -> T.Text
                    -> IO ()
example_updateAsset apiClient assetToUpdate = do
  rawUpdate <- T.getContents
  let definedIPs = T.lines rawUpdate
  _ <- updateDefinedIPs apiClient assetToUpdate definedIPs
  return ()

example_createAsset :: ApiClient
                    -> T.Text
                    -> IO ()
example_createAsset apiClient desiredAssetName = do
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
  (res, authSession) <- runRequest manager hostname unauthSession req
  let Just t = fmap createTokenResponseToken res
  return (t, authSession)

endSession :: ApiClient
           -> IO (Maybe (ApiResponse Object), CookieJar)
endSession apiClient = do
  let req = DeleteTokenRequest
            { deleteTokenRequestToken = apiClientToken apiClient
            }
  runApiRequest apiClient req

updateDefinedIPs :: ApiClient
                 -> T.Text
                 -> [T.Text]
                 -> IO (Maybe GetAssetByIdResponse)
updateDefinedIPs apiClient assetToUpdate definedIPs = do
  let req = UpdateDefinedIPsRequest
            { updateDefinedIPsAssetId = assetToUpdate
            , updateDefinedIPsToken = apiClientToken apiClient
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
            { createStaticAssetRequestToken = apiClientToken apiClient
            , createStaticAssetRequestName = assetName
            , createStaticAssetRequestDefinedIPs = definedIPs
            }
  (res, _) <- runApiRequest apiClient req
  return res

listAssets :: ApiClient
           -> Manager
           -> IO ()
listAssets apiClient manager = do
   let req = ListAssetsRequest
             { authenticationToken = apiClientToken apiClient
             }
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
             { getAssetByIdToken = apiClientToken apiClient
             , getAssetByIdId = assetToUpdate
             }
  (res, _) <- runApiRequest apiClient req
  return res

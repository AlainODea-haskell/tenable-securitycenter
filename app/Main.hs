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

import ApiClient
  ( createApiClient
  , ApiClient(..)
  , runApiRequest
  , endSession
  )

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

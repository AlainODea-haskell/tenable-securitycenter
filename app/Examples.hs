-- |
-- Module      : Examples
-- Copyright   : (c) 2017 Alain O'Dea
-- License     : Apache Public License, v. 2.0.
-- Maintainer  : Alain O'Dea <alain.odea@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Tenable SecurityCenter REST API - Usage Examples
--
-- Runnable examples of API usage.

{-# LANGUAGE OverloadedStrings #-}
module Examples
       ( listScans
       , listAssets
       , getAssetById
       , updateAsset
       , createAsset
       )
where

import ApiClient

import Network.Tenable.SecurityCenter.Scan
import Network.Tenable.SecurityCenter.Asset

import qualified Data.Text as T
import qualified Data.Text.IO as T

listScans :: ApiClient
          -> IO ()
listScans apiClient = do
   let req = ListScansRequest
   (res, _) <- runApiRequest apiClient req
   let usables = fmap scanUsable res
   print usables
   let manageables = fmap scanManageable res
   print manageables

listAssets :: ApiClient
           -> IO ()
listAssets apiClient = do
   let req = ListAssetsRequest
   (res, _) <- runApiRequest apiClient req
   let usables = fmap usableAssets res
   print usables
   let manageables = fmap manageableAssets res
   print manageables

getAssetById :: T.Text
             -> ApiClient
             -> IO (Maybe GetAssetByIdResponse)
getAssetById  assetToUpdate apiClient = do
  let req = GetAssetByIdRequest
             { getAssetByIdId = assetToUpdate
             }
  (res, _) <- runApiRequest apiClient req
  return res

updateAsset :: T.Text
            -> ApiClient
            -> IO ()
updateAsset assetToUpdate apiClient = do
  rawUpdate <- T.getContents
  let definedIPs = T.lines rawUpdate
  _ <- updateDefinedIPs apiClient assetToUpdate definedIPs
  return ()

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

createAsset :: T.Text
            -> ApiClient
            -> IO ()
createAsset desiredAssetName apiClient = do
  rawUpdate <- T.getContents
  let definedIPs = T.lines rawUpdate
  res <- createDefinedIPs apiClient desiredAssetName definedIPs
  let Just createdAssetId = fmap assetByIdId res
  T.putStrLn createdAssetId

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

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
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Either (either)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import           GHC.Exts (IsString(fromString))
import           Network.HTTP.Conduit
import           System.Environment (getArgs)
import           System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

main :: IO ()
main = example_updateAsset

example_updateAsset :: IO ()
example_updateAsset = do
  hSetBuffering stdout NoBuffering
  [configFilename, assetIdArg] <- getArgs
  configFile <- L8.readFile configFilename
  let config = either error id $ eitherDecode configFile
  let hostname = fromText . securityCenterHost $ config
  let u = securityCenterUsername config
  let p = securityCenterPassword config
  rawUpdate <- T.getContents
  let definedIPs = T.lines rawUpdate
  let assetToUpdate = fromString assetIdArg
  (t, session) <- getToken hostname u p
  _ <- updateDefinedIPs session hostname t assetToUpdate definedIPs
  _ <- endSession hostname (session, t)
  return ()

example_createAsset :: IO ()
example_createAsset = do
  hSetBuffering stdout NoBuffering
  [configFilename, assetIdArg] <- getArgs
  configFile <- L8.readFile configFilename
  let config = either error id $ eitherDecode configFile
  let hostname = fromText . securityCenterHost $ config
  let u = securityCenterUsername config
  let p = securityCenterPassword config
  rawUpdate <- T.getContents
  let definedIPs = T.lines rawUpdate
  let assetToUpdate = fromString assetIdArg
  (t, session) <- getToken hostname u p
  res <- createDefinedIPs session hostname t assetToUpdate definedIPs
  let Just createdAssetId = fmap assetByIdId res
  T.putStrLn createdAssetId
  _ <- endSession hostname (session, t)
  return ()

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

fromText :: IsString a
         => T.Text
         -> a
fromText = fromString . T.unpack

fromS8 :: IsString a
         => S8.ByteString
         -> a
fromS8 = fromString . S8.unpack

getToken :: S8.ByteString
         -> T.Text
         -> T.Text
         -> IO (Int, CookieJar)
getToken hostname u p = do
  let unauthSession = createCookieJar []
  let req = CreateTokenRequest { username = u, password = p }
  (res, authSession) <- runRequest hostname req unauthSession
  let Just t = fmap (token.response) res
  return (t, authSession)

updateDefinedIPs :: CookieJar
                 -> S8.ByteString
                 -> Int
                 -> S8.ByteString
                 -> [T.Text]
                 -> IO (Maybe GetAssetByIdResponse)
updateDefinedIPs session hostname t assetToUpdate definedIPs = do
  let req = UpdateDefinedIPsRequest
            { updateDefinedIPsAssetId = assetToUpdate
            , updateDefinedIPsToken = (fromString . show) t
            , updateDefinedIPsDefinedIPs = definedIPs
            }
  (res, _) <- runRequest hostname req session
  return $ fmap response res

createDefinedIPs :: CookieJar
                 -> S8.ByteString
                 -> Int
                 -> S8.ByteString
                 -> [T.Text]
                 -> IO (Maybe GetAssetByIdResponse)
createDefinedIPs session hostname t assetToUpdate definedIPs = do
  let req = CreateStaticAssetRequest
            { createStaticAssetRequestToken = (fromString . show) t
            , createStaticAssetRequestName = fromS8 assetToUpdate
            , createStaticAssetRequestDefinedIPs = definedIPs
            }
  (res, _) <- runRequest hostname req session
  return $ fmap response res

endSession :: S8.ByteString
           -> (CookieJar, Int)
           -> IO (Maybe (ApiResponse Object), CookieJar)
endSession hostname (session, t) = do
  let req = DeleteTokenRequest
            { deleteTokenRequestToken = (fromString . show) t
            }
  runRequest hostname req session

listAssets :: CookieJar
           -> S8.ByteString
           -> Int
           -> IO ()
listAssets session hostname t = do
   let req = ListAssetsRequest { authenticationToken = (fromString . show) t }
   (res, _) <- runRequest hostname req session
   let usables = fmap (usableAssets.response) res
   print usables
   let manageables = fmap (manageableAssets.response) res
   print manageables

getAssetById :: CookieJar
             -> S8.ByteString
             -> Int
             -> S8.ByteString
             -> IO (Maybe GetAssetByIdResponse)
getAssetById session hostname t assetToUpdate = do
  let req = GetAssetByIdRequest
             { getAssetByIdToken = (fromString . show) t
             , getAssetByIdId = assetToUpdate
             }
  (res, _) <- runRequest hostname req session
  return $ fmap response res

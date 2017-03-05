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

import           Control.Monad (liftM)
import           Data.Aeson
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text.IO as T
import qualified Data.Text as T
import           Network.HTTP.Conduit
import           System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Tenable SecurityCenter Host: "
  host <- S8.getLine
  putStr "Username: "
  u <- T.getLine
  putStr "Password: "
  p <- T.getLine
  (t, session) <- getToken host u p
  let assetToUpdate = "186"
  a <- getAssetById session host t assetToUpdate
  print a
  let Just definedIPs = (fmap (
                       (T.splitOn ",").
                       typeFieldsDefinedIPs.
                       assetByIdStaticTypeFields
                       ) $
                    (a :: Maybe GetAssetByIdResponse))
  print definedIPs
  updateDefinedIPs session host t assetToUpdate definedIPs

getToken :: S8.ByteString
         -> T.Text
         -> T.Text
         -> IO (Int, CookieJar)
getToken host u p = do
  let session = createCookieJar []
  let req = CreateTokenRequest { username = u, password = p }
  (res, session) <- runRequest host req session
  let Just t = fmap (token.response) res
  return (t, session)

listAssets :: CookieJar
           -> S8.ByteString
           -> Int
           -> IO ()
listAssets session host t = do
   let req = ListAssetsRequest { authenticationToken = (S8.pack . show) t }
   (res, session) <- runRequest host req session
   let usables = fmap (usableAssets.response) res
   print usables
   let manageables = fmap (manageableAssets.response) res
   print manageables

getAssetById :: CookieJar
             -> S8.ByteString
             -> Int
             -> S8.ByteString
             -> IO (Maybe GetAssetByIdResponse)
getAssetById session host t assetToUpdate = do
  let req = GetAssetByIdRequest
             { getAssetByIdToken = (S8.pack . show) t
             , getAssetByIdId = assetToUpdate
             }
  (res, session) <- runRequest host req session
  return $ fmap response res

updateDefinedIPs :: CookieJar
                 -> S8.ByteString
                 -> Int
                 -> S8.ByteString
                 -> [T.Text]
                 -> IO ()
updateDefinedIPs session host t assetToUpdate definedIPs = do
  let req = UpdateDefinedIPsRequest
            { updateDefinedIPsAssetId = assetToUpdate
            , updateDefinedIPsToken = (S8.pack . show) t
            , updateDefinedIPsDefinedIPs = definedIPs
            }
  (res, session) <- runRequest host req session
  let assetRes = fmap response res
  print (assetRes :: Maybe GetAssetByIdResponse)

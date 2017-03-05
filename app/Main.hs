-- |
-- Module      : Main
-- Copyright   : (c) 2016 Alain O'Dea
-- License     : Apache Public License, v. 2.0.
-- Maintainer  : Alain O'Dea <alain.odea@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Tenable SecurityCenter REST API - Asset updater example program
--
-- Example program that will update an asset with IPs from STDIN.

{-# LANGUAGE OverloadedStrings #-}
module Main where

import ApiClient
  ( createApiClient
  , ApiClient
  , endSession
  )
import Examples (updateAsset)

import qualified Data.Text as T
import           System.Environment (getArgs)
import           System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  (configFilename, assetIdArg) <- parseArgs
  apiClient <- createApiClient configFilename
  run_example (updateAsset assetIdArg) apiClient

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

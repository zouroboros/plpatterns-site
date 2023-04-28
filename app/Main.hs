{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment (lookupEnv)
import Data.Text.Lazy (pack)

import Server.Scotty (scottyServer)

main :: IO ()
main = do
    loadFile defaultConfig
    enablePlaygroundVar <- lookupEnv "ENABLEPLAYGROUND"
    allowOriginVar <- lookupEnv "ALLOWORIGIN"
    let enablePlayground = maybe False (== "TRUE") enablePlaygroundVar
    let allowOrigin = fmap pack allowOriginVar
    scottyServer enablePlayground allowOrigin

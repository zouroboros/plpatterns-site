{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment (lookupEnv, getEnv)
import Data.Text.Lazy (pack)

import Server.Scotty (scottyServer, ServerConfig (..), )

main :: IO ()
main = do
    loadFile defaultConfig
    enablePlaygroundVar <- lookupEnv "ENABLEPLAYGROUND"
    allowOriginVar <- lookupEnv "ALLOWORIGIN"
    portVar <- getEnv "PORT"
    dbPathVar <- getEnv "DBPATH"
    let port = read portVar
    let enablePlayground = Just "TRUE" == enablePlaygroundVar
    let allowOrigin = fmap pack allowOriginVar
    scottyServer (ServerConfig { 
        enablePlayground = enablePlayground,
        allowOrigin = allowOrigin,
        port = port,
        dbPath = dbPathVar })

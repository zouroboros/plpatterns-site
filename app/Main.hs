{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment (lookupEnv, getEnv)
import Data.Text.Lazy (pack)

import Server.Scotty (scottyServer)

main :: IO ()
main = do
    loadFile defaultConfig
    enablePlaygroundVar <- lookupEnv "ENABLEPLAYGROUND"
    allowOriginVar <- lookupEnv "ALLOWORIGIN"
    portVar <- getEnv "PORT"
    let port = read portVar
    let enablePlayground = Just "TRUE" == enablePlaygroundVar
    let allowOrigin = fmap pack allowOriginVar
    scottyServer enablePlayground allowOrigin port

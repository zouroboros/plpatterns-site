{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Server.Scotty (scottyServer)

main :: IO ()
main = scottyServer True (Just "*")

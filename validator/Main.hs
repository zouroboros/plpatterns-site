module Main (main) where

import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment (lookupEnv, getEnv)
import System.Exit (exitSuccess, exitFailure)

import DbValidator (validateDb)


main :: IO ()
main = do
    loadFile defaultConfig
    dbPathVar <- getEnv "DBPATH"
    validationResult <- validateDb dbPathVar
    if validationResult then do 
        putStrLn "db valid."
        exitSuccess
    else do
        putStrLn "db invalid."
        exitFailure
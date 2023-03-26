{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Server.Scotty (scottyServer) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (Identity (..))
import Data.Morpheus (httpPlayground)
import Data.Morpheus.App (runApp)
import Data.Morpheus.Types (App, render)
import qualified Server.Patterns as Patterns
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)
import Web.Scotty (ActionM, RoutePattern, ScottyM, body, get, param, post, raw, scottyApp)

isSchema :: ActionM String
isSchema = param "schema"

httpEndpoint :: RoutePattern -> App e IO -> ScottyM ()
httpEndpoint route app = do
  get route $ (isSchema *> raw (render app)) <|> raw httpPlayground
  post route $ raw =<< (liftIO . runApp app =<< body)

startServer :: ScottyM () -> IO ()
startServer app = do
  httpApp <- scottyApp app
  runSettings settings $ httpApp
  where
    settings = setPort 3000 defaultSettings

scottyServer :: IO ()
scottyServer = do
  startServer httpApp
  where
    httpApp :: ScottyM ()
    httpApp = do
      httpEndpoint "/patterns" Patterns.app
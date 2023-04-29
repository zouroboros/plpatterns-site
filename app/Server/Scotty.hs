{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Server.Scotty (scottyServer, ServerConfig(..)) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (Identity (..))
import Data.Morpheus (httpPlayground)
import Data.Morpheus.App (runApp)
import Data.Morpheus.Types (App, render, ResolverContext (config))
import qualified Server.Patterns as Patterns
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setPort)
import Web.Scotty (ActionM, RoutePattern, ScottyM, body, get, param, post, raw, scottyApp, addHeader)
import Data.ByteString.Lazy (ByteString)
import Web.Scotty.Trans (ActionT)
import Data.Text.Lazy (Text)
import Control.Monad (when)

import FileBasedDb(createDb)

data ServerConfig = ServerConfig {
  port :: Int,
  allowOrigin :: Maybe Text,
  dbPath :: FilePath,
  enablePlayground :: Bool
}

isSchema :: ActionM String
isSchema = param "schema"

httpEndpoint :: Bool -> Maybe Text -> RoutePattern -> App e IO -> ScottyM ()
httpEndpoint enablePlayground allowOrigin route app = do
  when enablePlayground $ get route $ (isSchema *> raw (render app)) <|> raw httpPlayground
  post route $ queryEndpoint allowOrigin app

queryEndpoint :: Maybe Text -> App e IO -> ActionM ()
queryEndpoint allowOrigin app = do
  let queryProcessor = liftIO . runApp app
  requestBody <- body
  maybe (return ()) (addHeader "Access-Control-Allow-Origin") allowOrigin 
  response <- queryProcessor requestBody
  raw response

startServer :: Int -> ScottyM () -> IO ()
startServer port app = do
  httpApp <- scottyApp app
  runSettings settings httpApp
  where
    settings = setPort port defaultSettings

scottyServer :: ServerConfig -> IO ()
scottyServer config = do
  startServer (port config) httpApp
  where
    httpApp :: ScottyM ()
    httpApp = do
      httpEndpoint (enablePlayground config) (allowOrigin config) "/patterns" 
        $ Patterns.app (createDb $ dbPath config)
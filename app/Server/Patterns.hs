{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Patterns(app) where

import Data.Text
import Data.Morpheus (App, deriveApp)
import Data.Morpheus.Types (GQLType, RootResolver(queryResolver), Undefined, defaultRootResolver, ResolverQ, liftEither)
import GHC.Generics (Generic)

data Language = Language { name :: Text, pattern :: [Pattern] } deriving (GQLType, Generic)

data LanguageArgs = LanuageArgs { name :: Maybe Text } deriving (GQLType, Generic)

data Pattern = Pattern { name :: Text, text :: Text, language :: Language } deriving (GQLType, Generic)

data PatternArgs = PatternArgs { name :: Maybe Text } deriving (GQLType, Generic)

data Query m = Query { pattern :: PatternArgs -> m [Pattern], languages :: LanguageArgs -> m [Language] } deriving (GQLType, Generic)

resolvePatterns :: PatternArgs -> ResolverQ e IO [Pattern]
resolvePatterns args = let
    javascript = Language { name = "Javascript", pattern = jsPattern }
    consoleLog = Pattern { name = "console.log", text = "console.log", language = javascript }
    consoleError = Pattern { name = "console.error", text = "console.error", language = javascript}
    jsPattern = [consoleLog, consoleError]
    in
    liftEither $ return $ Right jsPattern

resolveLanguages :: LanguageArgs -> ResolverQ e IO [Language]
resolveLanguages args = let
    javascript = Language { name = "Javascript", pattern = jsPattern }
    consoleLog = Pattern { name = "console.log", text = "console.log", language = javascript }
    consoleError = Pattern { name = "console.error", text = "console.error", language = javascript}
    jsPattern = [consoleLog, consoleError]
    in
    liftEither $ return $ Right [javascript]


rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver = defaultRootResolver { queryResolver = Query { pattern = resolvePatterns, languages = resolveLanguages } }

app :: App () IO
app = deriveApp rootResolver
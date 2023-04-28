{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Patterns(app) where

import Data.Text (Text, isInfixOf, unpack)
import Data.Morpheus (App, deriveApp)
import Data.Morpheus.Types (GQLType, RootResolver(queryResolver), Undefined, defaultRootResolver, ResolverQ, liftEither)
import GHC.Generics (Generic)
import System.FilePath ((</>))

import ExampleDb(ExampleDb(categories, examplesByCategory, exampleByCategoryAndName), Example(Example, id, name, description, example))
import FileBasedDb(createDb)
import Data.Maybe (catMaybes, fromMaybe)

data Language = Language { name :: Text } deriving (GQLType, Generic)

data LanguageArgs = LanguageArgs { name :: Maybe Text } deriving (GQLType, Generic)

data Pattern = Pattern { name :: Text, code :: Text, description :: Text, language :: Language } deriving (GQLType, Generic)

data PatternArgs = PatternArgs { language :: Text, name :: Maybe Text } deriving (GQLType, Generic)

data Query m = Query { languages :: LanguageArgs -> m [Language], pattern :: PatternArgs -> m [Pattern] } deriving (GQLType, Generic)

db :: ExampleDb IO FilePath Text
db = createDb "examples/examplebase"

resolveLanguages :: LanguageArgs -> ResolverQ e IO [Language]
resolveLanguages args = liftEither $ getLanguages args

resolvePattern :: PatternArgs -> ResolverQ e IO [Pattern]
resolvePattern args = liftEither $ getPattern args

getLanguages :: LanguageArgs -> IO (Either String [Language])
getLanguages LanguageArgs { name = query } = do
    allCategories <- categories db
    let filteredCategories = filter (\categoryName -> maybe True (`isInfixOf` categoryName) query) allCategories
    languages <- mapM getLanguage filteredCategories
    return $ Right $ catMaybes languages

getPattern :: PatternArgs -> IO (Either String [Pattern])
getPattern PatternArgs { language = language, name = Just name } = do
    example <- exampleByCategoryAndName db language name
    let pattern = fmap (toPattern Language { name = name }) example
    return $ maybe (Left "No example found") (\pattern -> Right [pattern]) pattern
getPattern PatternArgs { language = language, name = Nothing } = do
    examples <- examplesByCategory db language
    let pattern = fmap (map (toPattern Language { name = language })) examples
    return $ maybe (Left "No examples found") Right pattern

getLanguage :: Text -> IO (Maybe Language)
getLanguage name = do
    return $ Just Language { name = name }

toPattern :: Language -> Example FilePath Text -> Pattern
toPattern language Example { id = id, name = name, description = description, example = example } =
    Pattern { name = name, code = example, description = description, language = language }

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver = defaultRootResolver { queryResolver = Query { languages = resolveLanguages, pattern = resolvePattern } }

app :: App () IO
app = deriveApp rootResolver
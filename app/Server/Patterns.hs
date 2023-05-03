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

import ExampleDb(ExampleDb(..), 
    Example(Example, id, name, description, example))
import Data.Maybe (catMaybes, fromMaybe)

data Language = Language { name :: Text } deriving (GQLType, Generic)

data LanguageArgs = LanguageArgs { name :: Maybe Text } deriving (GQLType, Generic)

data Pattern = Pattern { name :: Text, code :: Text, description :: Text, language :: Language } deriving (GQLType, Generic)

data PatternArgs = PatternArgs { language :: Text, name :: Maybe Text } deriving (GQLType, Generic)

data SearchArgs = SearchArgs { searchFor :: Text } deriving (GQLType, Generic)

data SearchResult = PatternResult { pattern :: Pattern } deriving (GQLType, Generic)

data Query m = Query { 
    languages :: LanguageArgs -> m [Language], 
    pattern :: PatternArgs -> m [Pattern],
    search :: SearchArgs -> m [SearchResult] } deriving (GQLType, Generic)

type Db = ExampleDb IO FilePath Text

resolveLanguages :: Db -> LanguageArgs -> ResolverQ e IO [Language]
resolveLanguages db args = liftEither $ getLanguages db args

resolvePattern :: Db -> PatternArgs -> ResolverQ e IO [Pattern]
resolvePattern db args = liftEither $ getPattern db args

resolveSearch :: Db -> SearchArgs -> ResolverQ e IO [SearchResult]
resolveSearch db args = liftEither $ getSearchResults db args

getLanguages :: Db -> LanguageArgs -> IO (Either String [Language])
getLanguages db LanguageArgs { name = query } = do
    allCategories <- categories db
    let filteredCategories = filter (\categoryName -> maybe True (`isInfixOf` categoryName) query) allCategories
    languages <- mapM getLanguage filteredCategories
    return $ Right $ catMaybes languages

getPattern :: Db -> PatternArgs -> IO (Either String [Pattern])
getPattern db PatternArgs { language = language, name = Just name } = do
    example <- exampleByCategoryAndName db language name
    let pattern = fmap (toPattern Language { name = name }) example
    return $ maybe (Left "No example found") (\pattern -> Right [pattern]) pattern
getPattern db PatternArgs { language = language, name = Nothing } = do
    examples <- examplesByCategory db language
    let pattern = fmap (map (toPattern Language { name = language })) examples
    return $ maybe (Left "No examples found") Right pattern

getLanguage :: Text -> IO (Maybe Language)
getLanguage name = do
    return $ Just Language { name = name }

getSearchResults :: Db -> SearchArgs -> IO (Either String [SearchResult])
getSearchResults db SearchArgs { searchFor = searchFor } = do
    examples <- searchForExamples db searchFor
    let pattern = map (\(language, example) -> toPattern (Language { name = language }) example) examples
    let results = map PatternResult pattern
    return $ Right results

toPattern :: Language -> Example FilePath Text -> Pattern
toPattern language Example { id = id, name = name, description = description, example = example } =
    Pattern { name = name, code = example, description = description, language = language }

rootResolver :: Db -> RootResolver IO () Query Undefined Undefined
rootResolver db = defaultRootResolver { queryResolver = Query { 
    languages = resolveLanguages db, 
    pattern = resolvePattern db,
    search = resolveSearch db } }

app :: Db -> App () IO
app = deriveApp . rootResolver
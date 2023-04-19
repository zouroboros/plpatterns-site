{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Patterns(app) where

import Data.Text (Text)
import Data.Morpheus (App, deriveApp)
import Data.Morpheus.Types (GQLType, RootResolver(queryResolver), Undefined, defaultRootResolver, ResolverQ, liftEither)
import GHC.Generics (Generic)

import ExampleDb(ExampleDb(categories, examplesByCategory), Example(Example, id, name, description, example))
import FileBasedDb(createDb)
import Data.Maybe (catMaybes)

data Language = Language { name :: Text, pattern :: [Pattern] } deriving (GQLType, Generic)

data LanguageArgs = LanuageArgs { name :: Maybe Text } deriving (GQLType, Generic)

data Pattern = Pattern { name :: Text, code :: Text, language :: Language } deriving (GQLType, Generic)

data Query m = Query { languages :: LanguageArgs -> m [Language] } deriving (GQLType, Generic)

db :: ExampleDb IO FilePath Text
db = createDb "examples/examplebase"

resolveLanguages :: LanguageArgs -> ResolverQ e IO [Language]
resolveLanguages args = liftEither $ getLanguages args

getLanguages :: LanguageArgs -> IO (Either String [Language])
getLanguages args = do
    cats <- categories db
    languages <- mapM getLanguage cats
    return $ Right $ catMaybes languages

getLanguage :: Text -> IO (Maybe Language)
getLanguage name = do
   examples <- examplesByCategory db name
   return $ fmap (\patterns -> let lang = Language { name = name, pattern = map (toPattern lang) patterns } in lang) examples

toPattern :: Language -> Example FilePath Text -> Pattern
toPattern language Example { name = name, description = description, example = example } = 
    Pattern { name = name, code = description, language = language }

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver = defaultRootResolver { queryResolver = Query { languages = resolveLanguages } }

app :: App () IO
app = deriveApp rootResolver
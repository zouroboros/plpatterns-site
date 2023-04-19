{-# LANGUAGE DuplicateRecordFields #-}

module FileBasedDb where

import ExampleDb (ExampleDb(ExampleDb, categories, exampleById, examplesByCategory),
    Example(Example, id, name, example, description))
import System.Directory (listDirectory, doesDirectoryExist, makeAbsolute, findFile)
import System.FilePath (takeFileName, (</>))
import Data.Maybe (catMaybes)

createDb :: FilePath -> ExampleDb IO FilePath String
createDb basePath = ExampleDb {
    categories = FileBasedDb.categories basePath,
    examplesByCategory = FileBasedDb.examplesByCategory basePath,
    exampleById = FileBasedDb.exampleById basePath
}

categories :: FilePath -> IO [String]
categories basePath = do
    folders <- listDirectory basePath
    let names = map takeFileName folders
    return names

tryReadExample :: FilePath -> FilePath -> IO (Maybe (Example FilePath String))
tryReadExample basePath exampleId = do
    let examplePath = basePath </> exampleId
    absolutePath <- makeAbsolute examplePath
    exampleExists <- doesDirectoryExist absolutePath
    if exampleExists
        then do
            example <- readExample basePath exampleId
            return (Just example)
        else return Nothing

readExample :: FilePath -> FilePath -> IO (Example FilePath String)
readExample basePath exampleId = do
    let examplePath = basePath </> exampleId
    (name, info) <- readInfo $ examplePath </> "info"
    Just exampleCode <- findFile [examplePath] "example"
    example <- readFile exampleCode
    return Example {
        id = exampleId,
        name = name,
        description = info,
        example = exampleCode
    }

readInfo :: FilePath -> IO (String, String)
readInfo path = do
    content <- readFile path
    let name:_:description = lines content
    return (name, unlines description)

examplesByCategory :: FilePath -> String -> IO (Maybe [Example FilePath String])
examplesByCategory basePath category = do
    folders <- listDirectory $ basePath </> category
    let names = map takeFileName folders
    examples <- mapM (\name -> tryReadExample basePath (category </> name)) names
    return $ Just $ catMaybes examples

exampleById :: FilePath -> FilePath -> IO (Maybe (Example FilePath String))
exampleById = tryReadExample

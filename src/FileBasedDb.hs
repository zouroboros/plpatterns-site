{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module FileBasedDb where

import ExampleDb (ExampleDb(ExampleDb, categories, exampleById, examplesByCategory, exampleByCategoryAndName),
    Example(Example, id, name, example, description))
import System.Directory (listDirectory, doesDirectoryExist, makeAbsolute, findFile)
import System.FilePath (takeFileName, (</>), dropExtension)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Foldable

createDb :: FilePath -> ExampleDb IO FilePath T.Text
createDb basePath = ExampleDb {
    categories = FileBasedDb.categories basePath,
    examplesByCategory = FileBasedDb.examplesByCategory basePath,
    exampleById = FileBasedDb.exampleById basePath,
    exampleByCategoryAndName = FileBasedDb.exampleByCategoryAndName basePath
}

categories :: FilePath -> IO [T.Text]
categories basePath = do
    folders <- listDirectory basePath
    let names = map takeFileName folders
    return $ map T.pack names

tryReadExample :: FilePath -> FilePath -> IO (Maybe (Example FilePath T.Text))
tryReadExample basePath exampleId = do
    let examplePath = basePath </> exampleId
    absolutePath <- makeAbsolute examplePath
    exampleExists <- doesDirectoryExist absolutePath
    if exampleExists
        then do
            example <- readExample basePath exampleId
            return (Just example)
        else return Nothing

readExample :: FilePath -> FilePath -> IO (Example FilePath T.Text)
readExample basePath exampleId = do
    let examplePath = basePath </> exampleId
    files <- listDirectory examplePath
    (name, info) <- readInfo $ examplePath </> "info"
    let Just exampleFile = find (\file -> dropExtension file == "example") files
    exampleCode <- TIO.readFile $ examplePath </> exampleFile
    return Example {
        id = exampleId,
        name = name,
        description = info,
        example = exampleCode
    }

readInfo :: FilePath -> IO (T.Text, T.Text)
readInfo path = do
    content <- TIO.readFile path
    let name:_:description = T.lines content
    return (name, T.unlines description)

examplesByCategory :: FilePath -> T.Text -> IO (Maybe [Example FilePath T.Text])
examplesByCategory basePath category = do
    folders <- listDirectory $ basePath </> T.unpack category
    let names = map takeFileName folders
    examples <- mapM (\name -> tryReadExample basePath (T.unpack category </> name)) names
    return $ Just $ catMaybes examples

exampleById :: FilePath -> FilePath -> IO (Maybe (Example FilePath T.Text))
exampleById = tryReadExample

exampleByCategoryAndName :: FilePath -> T.Text -> T.Text -> IO (Maybe (Example FilePath T.Text))
exampleByCategoryAndName basePath category name = FileBasedDb.exampleById basePath (T.unpack category </> T.unpack (T.toLower (T.replace " " "-" name)))

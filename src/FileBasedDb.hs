{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module FileBasedDb where

import CMark (NodeType(DOCUMENT, TEXT), Node(Node), commonmarkToNode, nodeToCommonmark)
import ExampleDb (ExampleDb(ExampleDb, categories, exampleById, examplesByCategory, exampleByCategoryAndName),
    Example(Example, id, name, example, description))
import System.Directory (listDirectory, doesDirectoryExist, makeAbsolute, findFile)
import System.FilePath (takeFileName, (</>), dropExtension)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Foldable
import Data.List (isPrefixOf)

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
    let filteredNames = filter (not . ("." `isPrefixOf`)) names 
    return $ map T.pack filteredNames

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
    (name, info) <- readInfo $ examplePath </> "info.md"
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
    let (Node _ DOCUMENT (header:rest)) = commonmarkToNode [] content    
    return (nodeText header, 
        nodeToCommonmark [] Nothing (Node Nothing DOCUMENT rest))

nodeText :: Node -> T.Text
nodeText (Node _ (TEXT text) []) = text
nodeText (Node _ _ children) = T.concat $ map nodeText children

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

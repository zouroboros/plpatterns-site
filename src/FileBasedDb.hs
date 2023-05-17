{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module FileBasedDb where

import CMark (NodeType(DOCUMENT, TEXT, PARAGRAPH, IMAGE), Node(Node), commonmarkToNode, nodeToCommonmark)
import ExampleDb (ExampleDb(..), Example(..), ExamplePart(..))
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
    exampleByCategoryAndAlias = FileBasedDb.exampleByCategoryAndName basePath,
    searchForExamples = FileBasedDb.searchExamples basePath
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
    let alias = T.pack $ takeFileName examplePath
    files <- listDirectory examplePath
    (name, parts) <- readInfo examplePath (examplePath </> "info.md")
    return Example {
        id = exampleId,
        alias = alias,
        name = name,
        parts = parts
    }

readInfo :: FilePath -> FilePath -> IO (T.Text, [ExamplePart T.Text])
readInfo basePath path = do
    content <- TIO.readFile path
    let (Node _ DOCUMENT (header:rest)) = commonmarkToNode [] content 
    parts <- exampleParts basePath rest
    return (nodeText header, parts)

exampleParts :: FilePath -> [Node] -> IO [ExamplePart T.Text]
exampleParts basePath nodes = sequence $ map (nodeToExamplePart basePath) nodes

nodeToExamplePart :: FilePath -> Node -> IO (ExamplePart T.Text)
nodeToExamplePart basePath (Node _ PARAGRAPH [Node _ (IMAGE fileName title) _] ) = do
    code <- TIO.readFile $ basePath </> (T.unpack fileName)
    return Code { title = title, code = code }
nodeToExamplePart _ node = return (Markup { markup = nodeToCommonmark [] Nothing (Node Nothing DOCUMENT [node])})

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

searchExamples :: FilePath -> T.Text -> IO [(T.Text, Example FilePath T.Text)]
searchExamples path searchTerm = 
    let search (category, example) = T.toLower searchTerm `T.isInfixOf` T.toLower (name example) 
        retrieveCategory category = do examples <- FileBasedDb.examplesByCategory path category
                                       return $ examples >>= \examples -> return (category, examples)
    in do
    allCategories <- FileBasedDb.categories path
    examplesByCategoryMaybe <- mapM retrieveCategory allCategories
    let examplesByCategory = catMaybes examplesByCategoryMaybe
    let examplesAndCategory = concatMap (\(category, examples) -> map (\example -> (category, example)) examples)
                                examplesByCategory
    let results = filter search examplesAndCategory
    return results
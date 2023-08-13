module DbValidator where 

import Control.Exception (IOException, catch)

import ExampleDb
import FileBasedDb (createDb)

validateDb :: FilePath -> IO Bool
validateDb dbPath = do
    let db = createDb dbPath
    catch (validateDb' db) ((\e -> return False) :: IOException -> IO Bool)
    

validateDb' :: Monad md => ExampleDb md idType stringType -> md Bool
validateDb' db = do 
    categories <- categories db
    validationResults <- sequence $ map (validateCategory db) categories
    return $ and validationResults

validateCategory :: Monad md => ExampleDb md idType stringType -> stringType -> md Bool
validateCategory db category = do
    examples <- examplesByCategory db category
    let validationResult = maybe False (all validateExample) examples
    return validationResult

validateExample :: Example idType stringType -> Bool
validateExample example = True
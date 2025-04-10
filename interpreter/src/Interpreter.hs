module Interpreter where

import Parser
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import System.IO

 -- A table is a list of rows, each row is a list of strings
type Table = [[String]]


--Main interpret function
interpret :: Query -> IO ()
interpret (Query fromClause operations) = do
  -- Load data from file
  table <- loadData fromClause
  
  -- Apply operations in sequence (as a pipeline)
  let finalTable = foldl applyOperation table operations
  
  -- Print the result
  printTable finalTable


-- Load data from file specified in the FROM clause
loadData :: FromClause -> IO Table
loadData (From fileName) = loadCSV fileName


-- Load a CSV file into a Table
loadCSV :: String -> IO Table
loadCSV fileName = do
  putStrLn $ "Loading file: " ++ fileName
  exists <- doesFileExist fileName
  if not exists
    then do
      putStrLn $ "Warning: File " ++ fileName ++ " not found. Using empty table."
      return []
    else do
      content <- readFile fileName
      let rows = lines content
      return $ map parseCSVLine rows

-- Check if file exists
doesFileExist :: FilePath -> IO Bool
doesFileExist path = do
  result <- tryIOError (openFile path ReadMode)
  case result of
    Right handle -> do
      hClose handle
      return True
    Left _ -> 
      return False


-- Try IO action and catch errors
tryIOError :: IO a -> IO (Either IOError a)
tryIOError action = do
  result <- try action
  return result
  where
    try :: IO a -> IO (Either IOError a)
    try a = (Right <$> a) `catch` (return . Left)
    catch :: IO a -> (IOError -> IO a) -> IO a
    catch a handler = a `seq` a

-- Parse a CSV line into a list of values
parseCSVLine :: String -> [String]
parseCSVLine = splitOn ','

-- Split a string by a delimiter
splitOn :: Char -> String -> [String]
splitOn delim str = case break (== delim) str of
  (a, _:b) -> a : splitOn delim b
  (a, "")  -> [a]

-- Apply operation to table
applyOperation :: Table -> Operation -> Table
applyOperation table (Select indices) = 
  if null table then [] else
    let header = head table
        rows = tail table
    in [selectColumns indices row | row <- table]
applyOperation table (Filter condition) = 
  if null table then [] else
    let header = head table
        rows = tail table
    in header : filter (evaluateCondition header condition) rows


-- Select only specified columns from a row
selectColumns :: [Int] -> [String] -> [String]
selectColumns indices row = 
  if null indices
    then row  -- If no indices specified, return all
    else [row !! i | i <- indices, i >= 0 && i < length row]

-- Evaluate a condition on a row
evaluateCondition :: [String] -> Condition -> [String] -> Bool
evaluateCondition _ (Equals colIdx value) row = 
  if colIdx >= 0 && colIdx < length row then
    row !! colIdx == value
  else False
evaluateCondition _ (NotEquals colIdx value) row = 
  if colIdx >= 0 && colIdx < length row then
    row !! colIdx /= value
  else False

-- Print the table
printTable :: Table -> IO ()
printTable [] = putStrLn "Empty result"
printTable table = do
  -- Print each row as CSV
  mapM_ (putStrLn . L.intercalate ",") table
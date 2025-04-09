module Interpreter where

import Parser
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import System.IO

-- Data types to represent our data model
type ColumnName = String
type Value = String
type Row = M.Map ColumnName Value
type Table = [Row]

-- Environment to store identifiers
type Env = M.Map String String

-- Main interpreter function
interpret :: Query -> IO ()
interpret (Query fromClause whereClause operations) = do
  -- Load data from files
  table <- loadData fromClause
  
  -- Create an environment for evaluation
  let env = createEnv table
  
  -- Apply where clause if present
  let filteredTable = case whereClause of
                        Just condition -> filterByCondition env condition table
                        Nothing -> table
  
  -- Apply operations in sequence
  let finalTable = foldl (applyOperation env) filteredTable operations
  
  -- Print the result
  printTable finalTable

-- Create environment from table (for simplicity we'll just use the first row)
createEnv :: Table -> Env
createEnv [] = M.empty
createEnv (firstRow:_) = firstRow

-- Load data from files specified in the FROM clause
loadData :: FromClause -> IO Table
loadData (From fileNames) = do
  -- For simplicity, we'll assume files are CSV with header
  tables <- mapM loadCSV fileNames
  -- If multiple files, we could join them here
  return $ if null tables then [] else head tables

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
      if null rows
        then return []
        else do
          let headers = parseCSVLine (head rows)
              dataRows = map parseCSVLine (tail rows)
              table = map (makeRow headers) dataRows
          return table

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

-- Create a row from headers and values
makeRow :: [ColumnName] -> [Value] -> Row
makeRow headers values = M.fromList (zip headers values)

-- Filter table by condition
filterByCondition :: Env -> Condition -> Table -> Table
filterByCondition env condition = filter (evaluateCondition env condition)

-- Evaluate a condition on a row
evaluateCondition :: Env -> Condition -> Row -> Bool
evaluateCondition env (Equals id1 id2) row = 
  resolveValue env id1 row == resolveValue env id2 row
evaluateCondition env (NotEquals id1 id2) row = 
  resolveValue env id1 row /= resolveValue env id2 row

-- Resolve a value from either environment or row
resolveValue :: Env -> String -> Row -> String
resolveValue env id row = 
  case M.lookup id row of
    Just value -> value
    Nothing -> fromMaybe id (M.lookup id env)  -- Treat identifiers as literals if not found

-- Apply operation to table
applyOperation :: Env -> Table -> Operation -> Table
applyOperation env table (Select columns) = map (selectColumns columns) table
applyOperation env table (Filter condition) = filterByCondition env condition table

-- Select only specified columns from a row
selectColumns :: [ColumnName] -> Row -> Row
selectColumns columns row = 
  if null columns
    then row  -- If no columns specified, return all
    else M.filterWithKey (\k _ -> k `elem` columns) row

-- Print the table
printTable :: Table -> IO ()
printTable [] = putStrLn "Empty result"
printTable table = do
  let allColumns = L.nub $ concatMap M.keys table
  
  -- Print header
  putStrLn $ L.intercalate "," allColumns
  
  -- Print rows
  mapM_ (\row -> putStrLn $ L.intercalate "," 
         [fromMaybe "" (M.lookup col row) | col <- allColumns]) table
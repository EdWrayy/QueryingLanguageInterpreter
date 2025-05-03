module Interpreter where

import Parser
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import System.IO
import System.Directory
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import Data.Csv (encodeDefaultOrderedByName)

-- A table is a list of rows, each row is a list of strings
type Table = [[String]]

--Main interpret function
interpret :: Query -> IO ()
interpret (Query fromClause outputFile operations) = case fromClause of
    -- Case for a single file (From)
    From file -> do
        table <- loadCSV file
        let finalTable = foldl applyOperation table operations
        printTable finalTable
        outputResult outputFile finalTable

    -- Case for two files (FromPair)
    FromPair file1 file2 -> do
        table1 <- loadCSV file1
        table2 <- loadCSV file2
        let finalTable = foldl (applyOp2 table2) table1 operations
        printTable finalTable
        outputResult outputFile finalTable



-- Load a CSV file into a Table using cassava
loadCSV :: String -> IO Table
loadCSV fileName = do
  putStrLn $ "Loading file: " ++ fileName
  exists <- doesFileExist fileName
  if not exists
    then do
      putStrLn $ "File " ++ fileName ++ " not found. Query will be done with empty table."
      return []
    else do
      csvData <- BL.readFile fileName
      case Csv.decode Csv.NoHeader csvData of
        Left err -> do
          putStrLn $ "Error parsing CSV: " ++ err
          return []
        Right v -> 
          let table = map V.toList (V.toList v)
          in if validateUniformArity table
              then return table
              else do
                putStrLn $ "Error: File " ++ fileName ++ " has non-uniform arity."
                return []

--Check that each row has the same number of columns
validateUniformArity :: Table -> Bool
validateUniformArity [] = True
validateUniformArity (r:rs) = all (\row -> length row == length r) rs

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

-- Apply Operation when there are 2 files
applyOp2 :: Table -> Table -> Operation -> Table
applyOp2 secondary primary LeftMerge = leftMerge primary secondary
applyOp2 _ primary op = applyOperation primary op

-- Select only specified columns from a row
selectColumns :: [Int] -> [String] -> [String]
selectColumns indices row =
  if null indices
    then row  -- If no indices specified, return all
    else [row !! i | i <- indices, i >= 0 && i < length row]

-- Evaluate a condition on a row
evaluateCondition :: [String] -> Condition -> [String] -> Bool
evaluateCondition _ (Equals colIdx value) row =
  (colIdx >= 0 && colIdx < length row) && (row !! colIdx == value)
evaluateCondition _ (NotEquals colIdx value) row =
  (colIdx >= 0 && colIdx < length row) && (row !! colIdx /= value)

-- Print the table
printTable :: Table -> IO ()
printTable [] = putStrLn "Empty result"
printTable table = do
  -- Print each row as CSV
  mapM_ (putStrLn . L.intercalate ",") table

outputResult :: Maybe String -> Table -> IO ()
outputResult Nothing table = printTable table  -- Output to console if no output file
outputResult (Just fileName) table = writeTableToCSV fileName table


-- Write table to CSV using cassava
writeTableToCSV :: String -> Table -> IO ()
writeTableToCSV fileName table = do
    putStrLn $ "Writing output to file: " ++ fileName
    -- Convert Table to Vector of Vector String for cassava
    let csvData = Csv.encode $ map V.fromList table
        cleanData = removeTrailingNewline csvData --Renove trailing newline if present
    BL.writeFile fileName cleanData
    putStrLn $ "Output written to " ++ fileName

removeTrailingNewline :: BL.ByteString -> BL.ByteString
removeTrailingNewline bs
  | BL.null bs = bs
  | BL.last bs == 10 = BL.init bs  -- 10 is ASCII for newline
  | otherwise = bs

-- Merge two rows: prefer p's value unless it's empty
mergeRows :: [String] -> [String] -> [String]
mergeRows p q = zipWith choose p q
  where choose "" qVal = qVal
        choose pVal _  = pVal

-- Perform left merge based on the first column - could generalise it later
leftMerge :: Table -> Table -> Table
leftMerge p q =
  [ p1 : mergeRows (tail pRow) (tail qRow)
  | pRow@(p1:_) <- p
  , qRow@(q1:_) <- q
  , p1 == q1
  ]
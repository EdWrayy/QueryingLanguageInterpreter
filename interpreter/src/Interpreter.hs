module Interpreter where

import Parser
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe
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
    From file hasLabels -> do
        table <- loadCSV file
        if hasLabels then do
          let finalTable = foldl applyOperation table operations
          printTable finalTable
          outputResult outputFile finalTable
        else do 
          let width = length (head table)
          let tableWithDefaultHeaders = (map show [0 .. width - 1]) : table
          let computedTable = foldl applyOperation tableWithDefaultHeaders operations
          let finalTable = tail computedTable
          printTable finalTable
          outputResult outputFile finalTable

    -- Case for two files (FromPair)
    FromPair file1 file2 hasLabels -> do
      table1 <- loadCSV file1
      table2 <- loadCSV file2

      let (table1WithHeaders, table2WithHeaders) = if hasLabels
          then (table1, table2)
          else
            let width1 = length (head table1)
                width2 = length (head table2)
                t1h = (map show [0 .. width1 - 1]) : table1
                t2h = (map show [0 .. width2 - 1]) : table2
            in (t1h, t2h)

      let computedTable = foldl (applyOp2 table2WithHeaders) table1WithHeaders operations
      let finalTable = if hasLabels then computedTable else tail computedTable
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
applyOperation table (GroupBy colID aggFunc) = --Result will be a collapsed table, containing only the group with their corresponding results for the aggregation function
  if null table then [] else 
  let header = head table
      rows = tail table
      grouped = groupByColumn colID rows
      aggregated = map (aggregateGroup colID aggFunc) grouped
      newHeader = [header !! colID, show aggFunc]
    in newHeader : aggregated


-- Rename operation
applyOperation table (Rename idx newName) =
  case table of
    [] -> []
    (header:rows) ->
      let newHeader = updateAt idx newName header
      in newHeader : rows

-- Drop operation
applyOperation table (Drop indices) =
  let dropIndices = L.sort indices
  in map (dropAtIndices dropIndices) table

-- Sort operation
applyOperation table (Sort colIdx order) =
  case table of
    [] -> []
    (header:rows) ->
      let sortedRows = L.sortBy (compareRows colIdx order) rows
      in header : sortedRows

-- Add column operation
applyOperation table (AddColumn name defaultVal) =
  case table of
    [] -> [[name], [defaultVal]]
    (header:rows) ->
      let newHeader = header ++ [name]
          newRows = map (++ [defaultVal]) rows
      in newHeader : newRows

-- Append row operation
applyOperation table (AppendRow values) = table ++ [values]

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
evaluateCondition _ (LessThan colIdx val) row =
  case parseDouble (row !! colIdx) of
    Just x -> x < fromIntegral val
    Nothing -> False
evaluateCondition _ (GreaterThan colIdx val) row =
  case parseDouble (row !! colIdx) of
    Just x -> x > fromIntegral val
    Nothing -> False
evaluateCondition _ (LessThanEq colIdx val) row =
  case parseDouble (row !! colIdx) of
    Just x -> x <= fromIntegral val
    Nothing -> False
evaluateCondition _ (GreaterThanEq colIdx val) row =
  case parseDouble (row !! colIdx) of
    Just x -> x >= fromIntegral val
    Nothing -> False
evaluateCondition _ (And cond1 cond2) row =
  evaluateCondition row cond1 row && evaluateCondition row cond2 row
evaluateCondition _ (Or cond1 cond2) row =
  evaluateCondition row cond1 row || evaluateCondition row cond2 row
evaluateCondition _ (Not cond) row =
  not $ evaluateCondition row cond row



groupByColumn :: Int -> [[String]] -> [(String, [[String]])]
groupByColumn colID rows =
  let groups = L.groupBy (\a b -> a !! colID == b !! colID) $
               L.sortOn (!! colID) rows
  in [(head g !! colID, g) | g <- groups]

-- Aggregate a group of rows (supports both numeric and string operations)
aggregateGroup :: Int -> AggregateFunc -> (String, [[String]]) -> [String]
aggregateGroup groupColIdx aggFunc (groupKey, rows) =
  let 
    allValues = concatMap (extractValues groupColIdx) rows
      
    extractValues groupColIdx row = [row !! i | i <- [0..length row - 1], i /= groupColIdx]
      
    result = case aggFunc of
      Sum -> case mapMaybe parseDouble allValues of
        [] -> "0"
        nums -> show $ sum nums
                 
      Avg -> case mapMaybe parseDouble allValues of
        [] -> "0"
        nums -> show $ sum nums / fromIntegral (length nums)
                 
      Count -> show $ length rows
                 
      Min -> case allValues of
        [] -> ""
        vs -> minimum vs  -- Works for strings too (lexicographic order)
                 
      Max -> case allValues of
        [] -> ""
        vs -> maximum vs  -- Works for strings too (lexicographic order)
                 
      -- Additional string-specific aggregates
      Concat -> case allValues of
        [] -> ""
        vs -> concat vs   -- Concatenate all strings
                 
      ConcatDist -> case allValues of
        [] -> ""
        vs -> concat $ L.nub vs  -- Concatenate unique strings
    in [groupKey, result]

parseDouble :: String -> Maybe Double
parseDouble s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing



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

 -- Helper Functions

updateAt :: Int -> a -> [a] -> [a]
updateAt idx val xs = take idx xs ++ [val] ++ drop (idx + 1) xs

dropAtIndices :: [Int] -> [a] -> [a]
dropAtIndices indices row = [x | (i, x) <- zip [0..] row, i `notElem` indices]

compareRows :: Int -> SortOrder -> [String] -> [String] -> Ordering
compareRows idx Asc  = \r1 r2 -> compare (safeIndex idx r1) (safeIndex idx r2)
compareRows idx Desc = \r1 r2 -> compare (safeIndex idx r2) (safeIndex idx r1)

safeIndex :: Int -> [a] -> a
safeIndex i xs = if i < length xs then xs !! i else error "Index out of bounds"


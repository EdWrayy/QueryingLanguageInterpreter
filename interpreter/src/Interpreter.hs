module Interpreter where

import Parser
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as Set
import Data.List (dropWhileEnd)
import Data.Maybe
import Text.Read (readMaybe)
import Data.Char (toUpper, toLower, isSpace)
import System.IO
import System.Directory
import Debug.Trace (trace)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import Data.Csv (encodeDefaultOrderedByName)


-- A table is a list of rows, each row is a list of strings
type Table = [[String]]
type Row = [String]

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
        let (mergeOps, otherOps) = L.partition isMergeOp operations
        case mergeOps of 
            [mergeOp] -> do
                table1 <- loadCSV file1
                table2 <- loadCSV file2
                
                table1' <- if hasLabels 
                            then return table1 
                            else do
                              let w = length (head table1)
                              return $ (map show [0 .. w - 1]) : table1

                table2' <- if hasLabels 
                            then return table2 
                            else do
                              let w = length (head table2)
                              return $ (map show [0 .. w - 1]) : table2

                merged <- applyMergeOperation mergeOp table1' table2'  -- merged :: Table

                merged' <- if hasLabels
                            then return merged
                            else (do
                                  let mergedBody = tail merged
                                      mergedWidth = length (head mergedBody)
                                      newHeader = map show [0 .. mergedWidth - 1]
                                  return (newHeader : mergedBody))
                                  

               
                let computed = foldl applyOperation merged' otherOps
                    final = if hasLabels then computed else tail computed
                printTable final 
                outputResult outputFile final 
            [] -> do
                putStrLn "Error: No merge operation provided for two tables"
                return ()
            _ -> do
                putStrLn "Error: More than one merge operation specified"
                return ()


--identifies which merge operation to use
applyMergeOperation :: Operation -> Table -> Table -> IO Table
applyMergeOperation (InnerMerge cond) table1 table2 = return $ innerMerge cond table1 table2
applyMergeOperation (LeftMerge cond) table1 table2 = return $ leftMerge cond table1 table2
applyMergeOperation (RightMerge cond) table1 table2 = return $ rightMerge cond table1 table2
applyMergeOperation (OuterMerge cond) table1 table2 = return $ outerMerge cond table1 table2
applyMergeOperation _ _ _ = do
    putStrLn "Error: Invalid merge operation"
    return []


--checks that merge given is a correct merge operation
isMergeOp :: Operation -> Bool
isMergeOp (InnerMerge _) = True
isMergeOp (LeftMerge _) = True
isMergeOp (RightMerge _) = True
isMergeOp (OuterMerge _) = True
isMergeOp _ = False

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
--Set value operation
applyOperation table (Set rowIdx colIdx val) =
  let setRow rIdx row = if rIdx == rowIdx then updateAt colIdx val row else row
  in if null table then [] else
       let header = head table
           rows = tail table
           newRows = zipWith setRow [0..] rows
       in header : newRows
--Map operation
applyOperation table (Map colIdx funcName) =
  if null table then [] else
    let header = head table
        rows = tail table
        func = parseMapFunc funcName
        newRows = map (\row -> updateAt colIdx (func (row !! colIdx)) row) rows
    in header : newRows


parseMapFunc :: String -> (String -> String)
parseMapFunc opStr = 
  case words opStr of
    ["upper"] -> map toUpper
    ["lower"] -> map toLower
    ["add", nStr] -> 
      case readMaybe nStr :: Maybe Int of
        Just n  -> intMapOp (+ n)
        Nothing -> id
    ["sub", nStr] ->
      case readMaybe nStr :: Maybe Int of
        Just n  -> intMapOp (\x -> x - n)
        Nothing -> id
    ["mul", nStr] ->
      case readMaybe nStr :: Maybe Int of
        Just n  -> intMapOp (* n)
        Nothing -> id
    ["div", nStr] ->
      case readMaybe nStr :: Maybe Int of
        Just 0  -> id  -- avoid div by zero
        Just n  -> intMapOp (`div` n)
        Nothing -> id
    _ -> id  -- fallback: do nothing

intMapOp :: (Int -> Int) -> (String -> String)
intMapOp f s = case readMaybe s of
  Just x  -> show (f x)
  Nothing -> s



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
evaluateCondition _ (EqualsCol i j) row =
  i < length row && j < length row && (row !! i == row !! j)
evaluateCondition _ (NotEqualsCol i j) row =
  i < length row && j < length row && (row !! i /= row !! j)



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


--INNER MERGE: 2 input tables -> gives one result table
innerMerge :: Condition -> Table -> Table -> Table
--handles when there is a equals
innerMerge cond@(EqualsCol _ _) fstTable sndTable = innerMerge' cond fstTable sndTable
--handles case when there is notEquals
innerMerge cond@(NotEqualsCol _ _) fstTable sndTable = innerMerge' cond fstTable sndTable
--else error
innerMerge _ _ _ = error "innerMerge currently supports only EqualsCol and NotEqualsCol"


-- Inner Merge, Only include rows where the condition is met. These rows include all the data from both tables
innerMerge' :: Condition -> Table -> Table -> Table
innerMerge' cond fstTable sndTable =
  let newHeader = head fstTable ++  head sndTable
      fstRows = tail fstTable
      sndRows = tail sndTable
      mergedRows = mapMaybe (mergeIfMatch cond sndRows) fstRows
  in newHeader : mergedRows 


-- Left Merge, include all rows from the first table and only add data from the second table where the condition is met
leftMerge :: Condition -> Table -> Table -> Table
leftMerge _    [] _  = []              
leftMerge _    _  [] = []              
leftMerge cond fstTable sndTable =
  let newHeader = head fstTable ++ head sndTable
      fstRows   = tail fstTable
      sndRows   = tail sndTable
      blank     = blankRow (head sndTable)   

      -- either concatenate with match, or pad with blanks
      mergeOrPad :: Row -> Row
      mergeOrPad r1 =
        case mergeIfMatch cond sndRows r1 of
          Just merged -> merged
          Nothing     -> r1 ++ blank

      mergedRows = map mergeOrPad fstRows
  in newHeader : mergedRows


-- Right Merge, include all rows from the second table and only add data from the first table where the condition is met
rightMerge:: Condition -> Table -> Table -> Table
rightMerge _    [] _  = []              
rightMerge _    _  [] = []            
rightMerge cond fstTable sndTable =
  let newHeader = head fstTable ++ head sndTable
      fstRows   = tail fstTable
      sndRows   = tail sndTable
      blankLeft = blankRow (head fstTable)  

      -- for each right-table row, find a match on the left
      mergeOrPad :: Row -> Row
      mergeOrPad r2 =
        case L.find (\r1 -> checkCondition cond r1 r2) fstRows of
          Just r1 -> r1 ++ r2            
          Nothing -> blankLeft ++ r2     

      mergedRows = map mergeOrPad sndRows
  in newHeader : mergedRows

-- Outer Merge, include all rows from both tables and fill in blanks where the condition is not met
outerMerge :: Condition -> Table -> Table -> Table
outerMerge _    [] t2 = t2                 -- degenerate cases
outerMerge _    t1 [] = t1
outerMerge cond fstTable sndTable =
  let header        = head fstTable ++ head sndTable

      -- run the two directional merges (they already have headers)
      leftResult    = tail (leftMerge  cond fstTable sndTable)
      rightResult   = tail (rightMerge cond fstTable sndTable)

      mergedRows    = leftResult ++ rightResult

      -- de-duplicate while preserving order
      dedupRows     = L.nubBy rowEquals mergedRows
  in  header : dedupRows

rowEquals :: Row -> Row -> Bool
rowEquals = (==)          -- rows are just equal-length String lists


--Finds the first match from the second table that matches the condition with the first table
mergeIfMatch :: Condition -> [Row] -> Row -> Maybe Row
mergeIfMatch cond sndRows r1 =
  case L.find (checkCondition cond r1) sndRows of
    Just r2 -> Just (r1 ++ r2)   
    Nothing -> Nothing           
          

-- Helper for generating empty cells
blankRow :: Row -> Row
blankRow template = replicate (length template) ""

deduplicate :: [Row] -> [Row]
deduplicate = Set.toList . Set.fromList


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

--Used for checking column equality in merge operations
checkCondition :: Eq a => Condition -> [a] -> [a] -> Bool
checkCondition (EqualsCol i j) row1 row2 = 
    i < length row1 && j < length row2 && (row1 !! i) == (row2 !! j)
checkCondition (NotEqualsCol i j) row1 row2 = 
    i < length row1 && j < length row2 && (row1 !! i) /= (row2 !! j)
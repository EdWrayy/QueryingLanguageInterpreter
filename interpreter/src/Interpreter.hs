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
                merged <- applyMergeOperation mergeOp table1 table2
                let mergedWithNoDuplicates = head merged : L.nubBy rowEquals (tail merged) -- tried to Remove duplicates here
                    width = length (head mergedWithNoDuplicates)  
                    withHeaders = if hasLabels then mergedWithNoDuplicates else (map show [0 .. width - 1]) : mergedWithNoDuplicates
                    finalTable = foldl applyOperation withHeaders otherOps
                printTable finalTable
                outputResult outputFile finalTable
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

-- INNER MERGE: includes rows that match based on condition, excludes duplicates
-- Returns rows that satisfy the condition between both tables
-- Empty result if either table is empty
-- Removes duplicate rows in the result -- I USED SQL JOINS FOR REFERENCE, TASK 5 WAS MORE LIKE INNERJOIN SO WE WOULD SAY USE INNER JOIN TO DO TASK 5
innerMerge' :: Condition -> Table -> Table -> Table
innerMerge' cond fstTable sndTable
    | null fstTable || null sndTable = [] 
    | otherwise =
        let mergedRows = concatMap (joinWithScnd (tail sndTable)) (tail fstTable)
            header = mergeRows (head fstTable) (head sndTable)
        in header : remduplicate mergedRows
  where
    joinWithScnd :: [Row] -> Row -> [Row]
    joinWithScnd sRows pRow =
        [mergeRows pRow sRow | sRow <- sRows, checkCondition cond pRow sRow]

    remduplicate :: [Row] -> [Row]                                        
    remduplicate rows = Set.toList . Set.fromList $ map cleanUpRow rows

    cleanUpRow :: Row -> Row
    cleanUpRow = reverse . dropWhile (== "") . reverse       


--LEFT MERGE:  keeps all rows from P and merges matching rows from Q.
leftMerge :: Condition -> Table -> Table -> Table
leftMerge cond fstTable sndTable
    | null fstTable = []
    | otherwise =
        let pRows = tail fstTable
            sRows = tail sndTable
            merged = concatMap (processLeftMergeRow sRows cond) pRows
            header = mergeRows (head fstTable) (head sndTable)
        in header : remduplicate merged
  where
    processLeftMergeRow :: [Row] -> Condition -> Row -> [Row]
    processLeftMergeRow sRows cond pRow =
        let matches = filter (checkCondition cond pRow) sRows
        in if null matches
           then [mergeRows pRow []]
           else map (mergeRows pRow) matches

    remduplicate :: [Row] -> [Row]
    remduplicate rows = Set.toList . Set.fromList $ map cleanUpRow rows

    cleanUpRow :: Row -> Row
    cleanUpRow = reverse . dropWhile (== "") . reverse


--RIGHT MERGE: keeps all rows from Q and merges matching rows from P
rightMerge :: Condition -> Table -> Table -> Table
rightMerge cond fstTable sndTable =
    leftMerge (reverseCondition cond) sndTable fstTable
  where
    reverseCondition :: Condition -> Condition
    reverseCondition (EqualsCol i j) = EqualsCol j i
    reverseCondition (NotEqualsCol i j) = NotEqualsCol j i
    reverseCondition other = other


--OUTER MERGE: includes all rows from both P and Q, filling missing values from the other table.
outerMerge :: Condition -> Table -> Table -> Table
outerMerge cond fstTable sndTable
    | null fstTable = sndTable
    | null sndTable = fstTable
    | otherwise =
        let leftResults = tail $ leftMerge cond fstTable sndTable  -- exclude header
            rightResults = tail $ rightMerge cond fstTable sndTable  -- exclude header
            allMerged = leftResults ++ rightResults
            header = mergeRows (head fstTable) (head sndTable)
        in header : remduplicate allMerged
  where
    remduplicate :: [Row] -> [Row]
    remduplicate rows = Set.toList . Set.fromList $ map cleanUpRow rows

    cleanUpRow :: Row -> Row
    cleanUpRow = reverse . dropWhile (== "") . reverse


-- Helper to check if rows are equal (for filtering duplicates) - THERE ARE STILL DUPS
rowEquals :: Row -> Row -> Bool
rowEquals row1 row2 =
    length row1 == length row2 && and (zipWith (==) row1 row2)


-- Row merging function that takes values from primary row unless empty
mergeRows :: Row -> Row -> Row
mergeRows pRow sRow =
    let maxLen = max (length pRow) (length sRow)
        pPadded = pRow ++ replicate (maxLen - length pRow) ""
        sPadded = sRow ++ replicate (maxLen - length sRow) ""
    in zipWith (\p s -> if p == "" then s else p) pPadded sPadded



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
checkCondition (EqualsCol i j) row1 row2 = 
    i < length row1 && j < length row2 && (row1 !! i) == (row2 !! j)
checkCondition (NotEqualsCol i j) row1 row2 = 
    i < length row1 && j < length row2 && (row1 !! i) /= (row2 !! j)
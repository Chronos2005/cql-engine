module Interpreter where

import Lexer
import Parser
import System.IO
import Data.List
import Data.Maybe
import Control.Monad
import Text.CSV
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Type alias for CSV data
type CSVData = [[String]]

-- Type alias for environment (mapping source names to their data)
type Env = Map.Map String CSVData

-- Read a CSV file with the given name; treat every line as a row, splitting on commas
readCSVFile :: String -> IO CSVData
readCSVFile name = do
    let filename = "./" ++ name ++ ".csv"
    content <- readFile filename
    let rows    = lines content
        records = map (map trimWhitespace . splitCommas) rows
    return records

-- Simple CSV split (no support for quoted commas)
splitCommas :: String -> [String]
splitCommas [] = [""]
splitCommas s  = go s
  where
    go str =
      let (field, rest) = break (== ',') str in
      case rest of
        []     -> [field]
        (_:xs) -> field : go xs

-- Trim leading and trailing whitespace from a string
trimWhitespace :: String -> String
trimWhitespace = reverse . dropWhile isWhitespace . reverse . dropWhile isWhitespace
  where isWhitespace c = c `elem` " \t\n\r"

-- Main interpret function
interpret :: Query -> IO ()
interpret query = do
    -- Read CSV files
    env0 <- loadSources (fromSources query)
    
    -- Pad environment
    let env = padEnv query env0

    -- Execute query
    let result = executeQuery query env
    
    -- Output CSV
    putStr $ formatCSV result



-- Load all sources mentioned in the query
loadSources :: [Source] -> IO Env
loadSources sources = do
    pairs <- forM sources $ \source -> do
        let name = sourceName source
        data' <- readCSVFile name
        return (sourceAlias source, data')
    return $ Map.fromList pairs

-- Execute a query against the environment
executeQuery :: Query -> Env -> CSVData
executeQuery query env =
    let 
        -- Get all rows from the cartesian product of sources
        allRows = cartesianProduct (fromSources query) env
        
        -- Apply WHERE clause if present
        filteredRows = case whereClause query of
            Nothing -> allRows
            Just expr -> filter (\row -> evalBoolExpr expr row env) allRows
        
        -- Apply SELECT clause
        selectedRows = map (\row -> 
            map (\item -> evalExprToString (selectExpr item) row env) 
                (selectClause query)) filteredRows
        
        -- Apply ORDER BY clause if present
        sortedRows
          | null (orderByClause query) = sort selectedRows
          | otherwise                  = sortRows (orderByClause query) selectedRows env
    in
        sortedRows

-- Generate all rows from cartesian product of sources
cartesianProduct :: [Source] -> Env -> [[(String, String, Int, Int)]]
cartesianProduct sources env =
    let 
        sourcesWithData = map (\s -> 
            (sourceAlias s, fromMaybe [] (Map.lookup (sourceAlias s) env))) sources
        
        -- Generate initial state with just the first source
        initialProduct = case sourcesWithData of
            [] -> []
            (alias, rows):_ -> 
                [ [(alias, cell, rowIdx, colIdx) 
                  | (colIdx, cell) <- zip [1..] row ] 
                | (rowIdx, row) <- zip [1..] rows ]
        
        -- Function to add one more source to the product
        addSource prod (alias, rows) =
            [ row ++ [(alias, cell, rowIdx, colIdx) 
                     | (colIdx, cell) <- zip [1..] sourceRow ]
            | row <- prod, 
              (rowIdx, sourceRow) <- zip [1..] rows ]
    in
        foldl addSource initialProduct (drop 1 sourcesWithData)

-- Evaluate an expression to a boolean
evalBoolExpr :: Expr -> [(String, String, Int, Int)] -> Env -> Bool
evalBoolExpr expr row env =
    case expr of
        BinaryOp Eq e1 e2 -> evalExprToString e1 row env == evalExprToString e2 row env
        BinaryOp Neq e1 e2 -> evalExprToString e1 row env /= evalExprToString e2 row env
        BinaryOp Lt e1 e2 -> evalExprToString e1 row env < evalExprToString e2 row env
        BinaryOp Le e1 e2 -> evalExprToString e1 row env <= evalExprToString e2 row env
        BinaryOp Gt e1 e2 -> evalExprToString e1 row env > evalExprToString e2 row env
        BinaryOp Ge e1 e2 -> evalExprToString e1 row env >= evalExprToString e2 row env
        BinaryOp And e1 e2 -> evalBoolExpr e1 row env && evalBoolExpr e2 row env
        BinaryOp Or e1 e2 -> evalBoolExpr e1 row env || evalBoolExpr e2 row env
        UnaryOp Not e -> not (evalBoolExpr e row env)
        UnaryOp Exists e -> evalExprToString e row env /= ""
        _ -> error $ "Expression is not boolean: " ++ show expr

-- Evaluate an expression to a string
evalExprToString :: Expr -> [(String, String, Int, Int)] -> Env -> String
evalExprToString expr row env =
    case expr of
        ColumnRef table col ->
            let matches = filter (\(t, _, _, _) -> t == table) row
                colMatches = filter (\(_, _, _, c) -> c == (read col :: Int)) matches
            in case colMatches of
                (_, val, _, _):_ -> val
                _ -> error $ "Column not found: " ++ table ++ "." ++ col
        
        ColumnIndexRef table idx ->
            let matches = filter (\(t, _, _, c) -> t == table && c == idx) row
            in case matches of
                (_, val, _, _):_ -> val
                _ -> error $ "Column index not found: " ++ table ++ "." ++ show idx
        
        Identifier id ->
            let matches = filter (\(t, _, _, _) -> t == id) row
            in case matches of
                (_, val, _, _):_ -> val
                _ -> id  -- If not a column reference, treat as a literal
        
        StringLit s -> s
        
        IntLit i -> show i
        
        FunctionCall "COALESCE" [e1, e2] ->
            let v1 = evalExprToString e1 row env
            in if v1 /= "" then v1 else evalExprToString e2 row env
        
        BinaryOp Plus e1 e2 ->
            evalExprToString e1 row env ++ evalExprToString e2 row env
        
        _ -> error $ "Cannot evaluate expression to string: " ++ show expr

-- Sort rows according to ORDER BY clause
sortRows :: [(OrderItem, Bool)] -> CSVData -> Env -> CSVData
sortRows orderItems rows env =
    sortBy compareRows rows
  where
    compareRows r1 r2 = foldr combineComparisons EQ (zipWith compareItem orderItems [0..])
      where
        compareItem ((OrderItem expr), isAsc) i =
            let c = compare (evalOrderKey expr r1 env) (evalOrderKey expr r2 env)
            in if isAsc then c else invert c
        
        invert LT = GT
        invert GT = LT
        invert EQ = EQ
        
        combineComparisons EQ next = next
        combineComparisons result _ = result

-- Evaluate expression for ordering
evalOrderKey :: Expr -> [String] -> Env -> String
evalOrderKey _ _ _ = ""  

-- Format CSV data as a string
formatCSV :: CSVData -> String
formatCSV = unlines . map (intercalate "," . map escapeCSV)
  where
    escapeCSV s = s  -- For now, no escaping since we assume no commas in entries

-- Main function to run a query
runQuery :: String -> IO ()
runQuery input = do
    let tokens = alexScanTokens input
    let queryAst = parse tokens
    interpret queryAst



-- Pad each table's rows so every row has enough columns
padEnv :: Query -> Env -> Env
padEnv query env =
    let needed = maxIndices query
    in Map.mapWithKey (padTable needed) env

-- Pad one table's rows
padTable :: Map.Map String Int -> String -> CSVData -> CSVData
padTable needed tableName rows
    | all isTrulyEmpty rows = []
    | otherwise = case Map.lookup tableName needed of
        Nothing     -> rows
        Just maxIdx -> map (padRow maxIdx) rows
  where
    isTrulyEmpty row = all (== "") row


-- Pad one row to required number of columns
padRow :: Int -> [String] -> [String]
padRow n xs
    | length xs >= n = xs
    | otherwise      = xs ++ replicate (n - length xs) ""

-- Find the maximum column index needed for each table
maxIndices :: Query -> Map.Map String Int
maxIndices (Query froms mWhere selects orderBy) =
    let exprs = concat
          [ maybe [] pure mWhere
          , map selectExpr selects
          , map (orderExpr . fst) orderBy
          ]
        indices = concatMap extract exprs
    in foldl insert Map.empty indices
  where
    insert m (table, idx) = Map.insertWith max table idx m

-- Extract table + column references from an expression
extract :: Expr -> [(String, Int)]
extract expr = case expr of
    ColumnRef table col -> [(table, read col)]
    ColumnIndexRef table idx -> [(table, idx)]
    BinaryOp _ e1 e2 -> extract e1 ++ extract e2
    UnaryOp _ e -> extract e
    FunctionCall _ args -> concatMap extract args
    _ -> []

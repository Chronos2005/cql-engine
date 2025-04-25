module Interpreter where

import Lexer
import Parser
import System.IO
import Data.List
import Data.Maybe
import Control.Monad
import Text.CSV
import qualified Data.Map as Map

-- Type alias for CSV data
type CSVData = [[String]]

-- Type alias for environment (mapping source names to their data)
type Env = Map.Map String CSVData

-- Read a CSV file with the given name
readCSVFile :: String -> IO CSVData
readCSVFile name = do
    let filename = name ++ ".csv"
    content <- readFile filename
    case parseCSV filename content of
        Left err -> error $ "Error parsing CSV file: " ++ show err
        Right csv -> return $ map (map trimWhitespace) csv

-- Trim leading and trailing whitespace from a string
trimWhitespace :: String -> String
trimWhitespace = reverse . dropWhile isWhitespace . reverse . dropWhile isWhitespace
  where isWhitespace c = c `elem` " \t\n\r"

-- Main interpret function
interpret :: Query -> IO ()
interpret query = do
    -- Read all source files
    env <- loadSources (fromSources query)
    
    -- Execute the query
    let result = executeQuery query env
    
    -- Output the result
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
        sortedRows = case orderByClause query of
            [] -> selectedRows
            orderItems -> sortRows orderItems selectedRows env
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
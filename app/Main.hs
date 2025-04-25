module Main (
    main
  ) where


import Lexer 
import Parser
import System.Environment (getArgs)
import System.IO (readFile)
import Interpreter

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> processFile filename
        _          -> putStrLn "Usage: ./Main <filename>"

processFile :: FilePath -> IO ()
processFile filename = do
    content <- readFile filename
    runQuery content




module Main (
    main
  ) where


import System.Environment (getArgs)
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




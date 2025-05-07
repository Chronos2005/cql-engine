
module Main (
    main
  ) where


import System.Environment (getArgs)
import Interpreter ( runQuery )
import System.IO.Error (catchIOError)
import System.Exit (exitFailure)
import Control.Exception
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> processFile filename
        _          -> putStrLn "Usage: ./Main <filename>"

processFile :: FilePath -> IO ()
processFile filename = do
    content <- safeReadFile filename
    safeRunQuery content

safeReadFile :: FilePath -> IO String
safeReadFile path =
  catchIOError (readFile path) $ \_ -> do
    putStrLn $ "Error: Cannot open query file '" ++ path ++ "'. Please check the filename."
    exitFailure

safeRunQuery :: String -> IO ()
safeRunQuery input =
  catch
    (runQuery input)
    (\(ErrorCall msg) -> do
        putStrLn $ "Error: " ++ msg
        exitFailure
    )

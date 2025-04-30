{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}


module Spec (spec) where

import Test.Hspec
import System.IO (writeFile, hFlush, stdout)
import System.Directory (removeFile)
import System.IO.Silently (capture_)
import Interpreter
import Control.DeepSeq (force)
import Control.Exception (evaluate, catch, IOException)

spec :: Spec
spec = describe "Coursework Tasks using actual tN.cql files" $ do

  describe "Task 1 - Cartesian Product" $ do
    it "Example 1" $ do
      writeCSV "A.csv" "Aardvark, Aaron"
      writeCSV "B.csv" "Beagle , Brenda"
      runTest "t1.cql" "Aardvark,Aaron,Beagle,Brenda\n"
      clean ["A.csv", "B.csv"]

    it "Example 2" $ do
      writeCSV "A.csv" "1,2\n1,2"
      writeCSV "B.csv" "3,4\n3,4"
      runTest "t1.cql" (unlines ["1,2,3,4","1,2,3,4","1,2,3,4","1,2,3,4"])
      clean ["A.csv", "B.csv"]

    it "Example 3 (empty A)" $ do
      writeCSV "A.csv" ""
      writeCSV "B.csv" "foo,bar"
      runTest "t1.cql" ""
      clean ["A.csv", "B.csv"]

  describe "Task 2 - Permutation, Drop and Matching" $ do
    it "Example 1" $ do
      writeCSV "A.csv" "Brenda,Brenda,Beagle\nAaron,Aaron,Aardvark\nCiara,Caterpillar,Caterpillar"
      runTest "t2.cql" (unlines ["Aardvark,Aaron","Beagle,Brenda"])
      clean ["A.csv"]

    it "Example 2" $ do
      writeCSV "A.csv" "1,3,6\n1,2,2\n2,2,2\n2,2,6"
      runTest "t2.cql" (unlines ["2,2","6,2"])
      clean ["A.csv"]

    it "Example 3" $ do
      writeCSV "A.csv" "1,1,\n1,1,2\n5,4,3\n4,4,1\n,,5"
      runTest "t2.cql" (unlines [",1","1,4","2,1","5,"])
      clean ["A.csv"]

  describe "Task 3 - Existence Check" $ do
    it "Example 1" $ do
      writeCSV "A.csv" "Ciara, Caterpillar\nAaron, Aardvark\nBrenda ,Beagle"
      runTest "t3.cql" (unlines ["Aaron,Aardvark","Brenda,Beagle","Ciara,Caterpillar"])
      clean ["A.csv"]

    it "Example 2" $ do
      writeCSV "A.csv" "Eric ,Eagle\nDorothy,\nAaron,\nCiara,Caterpillar\nBrenda,Beagle"
      runTest "t3.cql" (unlines ["Brenda,Beagle","Ciara,Caterpillar","Eric,Eagle"])
      clean ["A.csv"]

  describe "Task 4 - Copying and Constants" $ do
    it "Example 1" $ do
      writeCSV "A.csv" "Brenda\nEric\nCiara"
      runTest "t4.cql" (unlines ["Brenda,foo,Brenda","Ciara,foo,Ciara","Eric,foo,Eric"])
      clean ["A.csv"]

    it "Example 2" $ do
      writeCSV "A.csv" "Eric\n\nBrenda"
      runTest "t4.cql" (unlines [",foo,","Brenda,foo,Brenda","Eric,foo,Eric"])
      clean ["A.csv"]

  describe "Task 5 - Left Merge on First Column" $ do
    it "Example 1" $ do
      writeCSV "P.csv" "1,5,4,\n2,,2,\n3,7,1,2\n4,8,,"
      writeCSV "Q.csv" "1,6,4,7\n2,8,5,3\n2,,,1\n4,,2,3"
      runTest "t5.cql" (unlines ["1,5,4,7","2,,2,1","2,8,2,3","4,8,2,3"])
      clean ["P.csv", "Q.csv"]

    it "Example 2" $ do
      writeCSV "P.csv" "Aaron,foo,,baz\nBrenda,,bar,baz"
      writeCSV "Q.csv" "Brenda,foo,,\nAaron,,bar,"
      runTest "t5.cql" (unlines ["Aaron,foo,bar,baz","Brenda,foo,bar,baz"])
      clean ["P.csv", "Q.csv"]

    it "Example 3 (no matches)" $ do
      writeCSV "P.csv" "1,6,2,3\n2,7,4,5"
      writeCSV "Q.csv" "3,8,6,6\n4,9,5,3"
      runTest "t5.cql" ""
      clean ["P.csv", "Q.csv"]

--------------------------------------------------------------------------------

-- Helper to write and flush CSV files
writeCSV :: FilePath -> String -> IO ()
writeCSV path content = do
  writeFile path content
  hFlush stdout

runTest :: FilePath -> String -> IO ()
runTest queryFile expectedOutput = do
  query <- readFile queryFile
  output <- capture_ (runQuery query)
  evaluate (force output) >>= (`shouldBe` expectedOutput)  -- Fully force and compare before cleanup

clean :: [FilePath] -> IO ()
clean = mapM_ safeRemove
  where 
    safeRemove :: FilePath -> IO ()
    safeRemove path = removeFile path `catch` \(e :: IOException) -> 
      putStrLn $ "Warning: Could not remove " ++ path ++ ": " ++ show e
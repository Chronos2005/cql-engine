{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Spec (spec) where

import Test.Hspec
import System.IO (writeFile, hFlush, stdout)
import System.Directory (removeFile)
import System.IO.Silently (capture_)
import Interpreter
import Control.DeepSeq (force)
import Control.Exception (evaluate, catch, IOException)
import Data.List (sort)

spec :: Spec
spec = describe "Coursework Tasks using actual tN.cql files" $ do

  describe "Task 1 - Cartesian Product" $ do
    it "Example 1 - Basic case" $ do
      writeCSV "A.csv" "Aardvark, Aaron"
      writeCSV "B.csv" "Beagle , Brenda"
      runTest "t1.cql" "Aardvark,Aaron,Beagle,Brenda\n"
      clean ["A.csv", "B.csv"]

    it "Example 2 - Multiple rows" $ do
      writeCSV "A.csv" "1,2\n1,2"
      writeCSV "B.csv" "3,4\n3,4"
      runTest "t1.cql" (unlines ["1,2,3,4","1,2,3,4","1,2,3,4","1,2,3,4"])
      clean ["A.csv", "B.csv"]

    it "Example 3 - Empty A" $ do
      writeCSV "A.csv" ""
      writeCSV "B.csv" "foo,bar"
      runTest "t1.cql" ""
      clean ["A.csv", "B.csv"]
      
    it "Empty B" $ do
      writeCSV "A.csv" "foo,bar"
      writeCSV "B.csv" ""
      runTest "t1.cql" ""
      clean ["A.csv", "B.csv"]
      
    it "Both empty" $ do
      writeCSV "A.csv" ""
      writeCSV "B.csv" ""
      runTest "t1.cql" ""
      clean ["A.csv", "B.csv"]
      
    it "With empty strings" $ do
      writeCSV "A.csv" ",\n1,"
      writeCSV "B.csv" "3,\n,4"
      runTest "t1.cql" (unlines [",,3,",",,4","1,,3,","1,,4"])
      clean ["A.csv", "B.csv"]
      
    it "With many rows" $ do
      writeCSV "A.csv" "a,b\nc,d"
      writeCSV "B.csv" "1,2\n3,4\n5,6"
      runTest "t1.cql" (unlines [
        "a,b,1,2","a,b,3,4","a,b,5,6",
        "c,d,1,2","c,d,3,4","c,d,5,6"])
      clean ["A.csv", "B.csv"]

  describe "Task 2 - Permutation, Drop and Matching" $ do
    it "Example 1 - Basic match" $ do
      writeCSV "A.csv" "Brenda,Brenda,Beagle\nAaron,Aaron,Aardvark\nCiara,Caterpillar,Caterpillar"
      runTest "t2.cql" (unlines ["Aardvark,Aaron","Beagle,Brenda"])
      clean ["A.csv"]

    it "Example 2 - Numbers match" $ do
      writeCSV "A.csv" "1,3,6\n1,2,2\n2,2,2\n2,2,6"
      runTest "t2.cql" (unlines ["2,2","6,2"])
      clean ["A.csv"]

    it "Example 3 - Empty strings" $ do
      writeCSV "A.csv" "1,1,\n1,1,2\n5,4,3\n4,4,1\n,,5"
      runTest "t2.cql" (unlines [",1","1,4","2,1","5,"])
      clean ["A.csv"]
      
    it "All rows match" $ do
      writeCSV "A.csv" "a,a,1\nb,b,2\nc,c,3"
      runTest "t2.cql" (unlines ["1,a","2,b","3,c"])
      clean ["A.csv"]
      
    it "No rows match" $ do
      writeCSV "A.csv" "a,b,1\nc,d,2\ne,f,3"
      runTest "t2.cql" ""
      clean ["A.csv"]
      
    it "Empty input" $ do
      writeCSV "A.csv" ""
      runTest "t2.cql" ""
      clean ["A.csv"]
      
    it "Empty strings match" $ do
      writeCSV "A.csv" ",,1\n,,2\na,a,3"
      runTest "t2.cql" (unlines ["1,","2,","3,a"])
      clean ["A.csv"]

  describe "Task 3 - Existence Check" $ do
    it "Example 1 - Basic case" $ do
      writeCSV "A.csv" "Ciara, Caterpillar\nAaron, Aardvark\nBrenda ,Beagle"
      runTest "t3.cql" (unlines ["Aaron,Aardvark","Brenda,Beagle","Ciara,Caterpillar"])
      clean ["A.csv"]

    it "Example 2 - Mixed values" $ do
      writeCSV "A.csv" "Eric ,Eagle\nDorothy,\nAaron,\nCiara,Caterpillar\nBrenda,Beagle"
      runTest "t3.cql" (unlines ["Brenda,Beagle","Ciara,Caterpillar","Eric,Eagle"])
      clean ["A.csv"]
      
    it "All empty second column" $ do
      writeCSV "A.csv" "Eric,\nDorothy,\nAaron,"
      runTest "t3.cql" ""
      clean ["A.csv"]
      
    it "All non-empty second column" $ do
      writeCSV "A.csv" "Eric,Eagle\nDorothy,Dove\nAaron,Aardvark"
      runTest "t3.cql" (unlines ["Aaron,Aardvark","Dorothy,Dove","Eric,Eagle"])
      clean ["A.csv"]
      
    it "Empty input" $ do
      writeCSV "A.csv" ""
      runTest "t3.cql" ""
      clean ["A.csv"]
      
    it "Empty first column" $ do
      writeCSV "A.csv" ",Eagle\n,Dove\n,Aardvark"
      runTest "t3.cql" (unlines [",Aardvark",",Dove",",Eagle"])
      clean ["A.csv"]

  describe "Task 4 - Copying and Constants" $ do
    it "Example 1 - Basic case" $ do
      writeCSV "A.csv" "Brenda\nEric\nCiara"
      runTest "t4.cql" (unlines ["Brenda,foo,Brenda","Ciara,foo,Ciara","Eric,foo,Eric"])
      clean ["A.csv"]

    it "Example 2 - With empty string" $ do
      writeCSV "A.csv" "Eric\n\nBrenda"
      runTest "t4.cql" (unlines [",foo,","Brenda,foo,Brenda","Eric,foo,Eric"])
      clean ["A.csv"]
      
    it "Only empty strings" $ do
      writeCSV "A.csv" "\n\n"
      runTest "t4.cql" (unlines [",foo,",",foo,"])
      clean ["A.csv"]
      
    it "Empty input" $ do
      writeCSV "A.csv" ""
      runTest "t4.cql" ""
      clean ["A.csv"]
      
    it "Mixed values with spaces" $ do
      writeCSV "A.csv" " Eric \n  \n Brenda "
      runTest "t4.cql" (unlines [",foo,","Brenda,foo,Brenda","Eric,foo,Eric"])
      clean ["A.csv"]
      
    it "Special characters" $ do
      writeCSV "A.csv" "123\n!@#\n"
      runTest "t4.cql" (unlines ["!@#,foo,!@#",",foo,","123,foo,123"])
      clean ["A.csv"]

  describe "Task 5 - Left Merge on First Column" $ do
    it "Example 1 - Basic merge" $ do
      writeCSV "P.csv" "1,5,4,\n2,,2,\n3,7,1,2\n4,8,,"
      writeCSV "Q.csv" "1,6,4,7\n2,8,5,3\n2,,,1\n4,,2,3"
      runTest "t5.cql" (unlines ["1,5,4,7","2,,2,1","2,8,2,3","4,8,2,3"])
      clean ["P.csv", "Q.csv"]

    it "Example 2 - String values" $ do
      writeCSV "P.csv" "Aaron,foo,,baz\nBrenda,,bar,baz"
      writeCSV "Q.csv" "Brenda,foo,,\nAaron,,bar,"
      runTest "t5.cql" (unlines ["Aaron,foo,bar,baz","Brenda,foo,bar,baz"])
      clean ["P.csv", "Q.csv"]

    it "Example 3 - No matches" $ do
      writeCSV "P.csv" "1,6,2,3\n2,7,4,5"
      writeCSV "Q.csv" "3,8,6,6\n4,9,5,3"
      runTest "t5.cql" ""
      clean ["P.csv", "Q.csv"]
      
    it "Multiple matches per key" $ do
      writeCSV "P.csv" "1,5,4,\n1,9,8,7\n2,,2,"
      writeCSV "Q.csv" "1,6,4,7\n1,0,0,0\n2,8,5,3"
      runTest "t5.cql" (unlines [
        "1,5,4,7","1,9,8,7",
        "1,5,4,0","1,9,8,0",
        "2,,2,3"])
      clean ["P.csv", "Q.csv"]
      
    it "Empty P" $ do
      writeCSV "P.csv" ""
      writeCSV "Q.csv" "1,6,4,7\n2,8,5,3"
      runTest "t5.cql" ""
      clean ["P.csv", "Q.csv"]
      
    it "Empty Q" $ do
      writeCSV "P.csv" "1,5,4,\n2,,2,"
      writeCSV "Q.csv" ""
      runTest "t5.cql" ""
      clean ["P.csv", "Q.csv"]
      
    it "All empty strings" $ do
      writeCSV "P.csv" ",,,"
      writeCSV "Q.csv" ",1,2,3"
      runTest "t5.cql" (unlines [",,1,2,3"])
      clean ["P.csv", "Q.csv"]
      
    it "Empty strings in key position" $ do
      writeCSV "P.csv" ",a,b,c\n1,d,e,f"
      writeCSV "Q.csv" ",g,h,i\n2,j,k,l"
      runTest "t5.cql" (unlines [",a,b,c"])
      clean ["P.csv", "Q.csv"]

--------------------------------------------------------------------------------

-- Helper to write and flush CSV files
writeCSV :: FilePath -> String -> IO ()
writeCSV path content = do
  writeFile path content
  --hFlush stdout

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
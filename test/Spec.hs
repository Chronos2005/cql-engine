{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec
import System.Process (readProcessWithExitCode)
import System.Exit     (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | Helper to run the CQL interpreter on given .cql program and CSV inputs

runCql :: FilePath -> FilePath -> [(String, T.Text)] -> IO T.Text
runCql exe prog inputs = withSystemTempDirectory "cql-test" $ \dir -> do
  -- write CSVs omitted for brevity...
  (code, out, err) <- readProcessWithExitCode exe [prog] ""
  case code of
    ExitSuccess   -> return (T.pack out)
    ExitFailure _ -> error $ "interpreter failed:\nstdout:\n" ++ out ++ "\nstderr:\n" ++ err

main :: IO ()
main = hspec $ do
  let exe = "PLC-Coursework-exe"  -- adjust to your interpreter name/path

  describe "Task 1: Cartesian Product" $ do
    it "produces product for non-empty inputs" $ do
      let a = [("A", "Aardvark, Aaron"),("B","Beagle , Brenda")]
          csvA = T.unlines $ map snd a
          csvB = "foo,bar\n"
      out <- runCql exe "t1.cql" [("A", csvA), ("B", csvB)]
      let expected = T.unlines ["Aardvark,Aaron,foo,bar", "Beagle,Brenda,foo,bar"]
      out `shouldBe` expected

  describe "Task 2: Permutation, Drop and Matching" $ do
    it "selects and permutes matching rows" $ do
      let csv = T.unlines ["Brenda,Brenda,Beagle", "Aaron,Aaron,Aardvark", "Ciara,Caterpillar,Caterpillar"]
      out <- runCql exe "t2.cql" [("A", csv)]
      let expected = T.unlines ["Aardvark,Aaron", "Beagle,Brenda"]
      out `shouldBe` expected

  describe "Task 3: Existence Check" $ do
    it "filters out rows with empty second column" $ do
      let csv = T.unlines ["Eric ,Eagle", "Dorothy,", "Aaron,", "Ciara,Caterpillar", "Brenda,Beagle"]
      out <- runCql exe "t3.cql" [("A", csv)]
      let expected = T.unlines ["Brenda,Beagle", "Ciara,Caterpillar", "Eric,Eagle"]
      out `shouldBe` expected

  describe "Task 4: Copying and Constants" $ do
    it "adds constant and duplicates column" $ do
      let csv = T.unlines ["Brenda", "Eric", "Ciara"]
      out <- runCql exe "t4.cql" [("A", csv)]
      let expected = T.unlines ["Brenda,foo,Brenda", "Ciara,foo,Ciara", "Eric,foo,Eric"]
      out `shouldBe` expected

  describe "Task 5: Left merge on first column" $ do
    it "merges P and Q with left merge logic" $ do
      let p = T.unlines ["1,5,4,", "2,,2,", "3,7,1,2", "4,8,,"]
          q = T.unlines ["1,6,4,7", "2,8,5,3", "2,,,1", "4,,2,3"]
      out <- runCql exe "t5.cql" [("P", p), ("Q", q)]
      let expected = T.unlines ["1,5,4,7", "2,,2,1", "2,8,2,3", "4,8,2,3"]
      out `shouldBe` expected

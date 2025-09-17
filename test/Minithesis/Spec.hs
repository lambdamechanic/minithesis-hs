module Minithesis.Spec (spec) where

import Control.Exception (Exception, throwIO)
import Control.Monad (forM_, when)
import Data.IORef
import Minithesis
import System.Directory (removeFile)
import System.IO
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Test.Hspec

spec :: Spec
spec = do
  describe "TestCase.forChoices" $ do
    it "raises Frozen when interacting with a completed test case" $ do
      tc <- forChoices [0] False
      setStatus tc Valid
      markStatus tc Interesting `shouldThrow` isFrozen
      choice tc 10 `shouldThrow` isFrozen
      forcedChoice tc 10 `shouldThrow` isFrozen
  describe "TestCase.choice" $ do
    it "rejects bounds that exceed 64 bits" $ do
      tc <- forChoices [] False
      choice tc (2 ^ (64 :: Integer)) `shouldThrow` isValueError
  describe "runTest" $ do
    it "can choose the full 64-bit range" $ do
      let opts = defaultRunOptions {runQuiet = True}
      runTest opts $ \tc -> do
        _ <- choice tc (2 ^ (64 :: Integer) - 1)
        pure ()

    it "raises Unsatisfiable for an unbounded test function when the buffer is small" $ do
      let opts = defaultRunOptions {runQuiet = True, runBufferSize = 8}
          action =
            runTest opts $ \tc ->
              let loop = do
                    _ <- choice tc 10
                    loop
               in loop
      action `shouldThrow` isUnsatisfiable
  describe "weighted" $ do
    it "prints a top-level weighted" $ do
      tc <- forChoices [] True
      (out, res) <- captureStdout $ do
        b <- weighted tc 0.5
        pure b
      out `shouldBe` "weighted(0.5): False\n"
      res `shouldBe` False

    it "impossible weighted still allows later Failure" $ do
      let opts = defaultRunOptions {runQuiet = True}
          action =
            runTest opts $ \tc -> do
              _ <- choice tc 1
              forM_ [1 .. (10 :: Int)] $ \_ -> do
                w <- weighted tc 0.0
                when w (throwIO Failure)
              v <- choice tc 1
              when (v == 1) (throwIO Failure)
      action `shouldThrow` isFailure
    it "satisfies preconditions when using assume" $ do
      let opts = defaultRunOptions {runQuiet = True}
      runTest opts $ \tc -> do
        n <- choice tc 9
        assume tc (n /= 0)
        n `shouldSatisfy` (/= 0)
    it "raises Unsatisfiable when all test cases are rejected" $ do
      let opts = defaultRunOptions {runQuiet = True}
          action =
            runTest opts $ \tc -> do
              _ <- choice tc 9
              assume tc False
      action `shouldThrow` isUnsatisfiable

isFrozen :: Frozen -> Bool
isFrozen _ = True

isValueError :: ValueError -> Bool
isValueError _ = True

isUnsatisfiable :: Unsatisfiable -> Bool
isUnsatisfiable _ = True

-- Local helpers for tests
data Failure = Failure deriving (Show)

instance Exception Failure

isFailure :: Failure -> Bool
isFailure _ = True

captureStdout :: IO a -> IO (String, a)
captureStdout action = do
  -- Save original stdout
  oldStdout <- hDuplicate stdout
  hSetBuffering stdout LineBuffering
  (path, h) <- openTempFile "." "minithesis-stdout.txt"
  hSetBuffering h LineBuffering
  hDuplicateTo h stdout
  result <- action
  hFlush stdout
  hDuplicateTo oldStdout stdout
  hClose h
  hClose oldStdout
  out <- readFile path
  removeFile path
  pure (out, result)

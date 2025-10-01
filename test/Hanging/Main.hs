module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, void)
import Minithesis
  ( RunOptions,
    TestCase,
    Unsatisfiable (..),
    assume,
    choice,
    defaultRunOptions,
    resolveRunOptions,
    runBufferSize,
    runQuiet,
  )
import Minithesis.Property as MP
import Test.Syd hiding (runTest)
import Test.Syd.OptParse (Settings (..), Threads (..), defaultSettings)
import Test.Syd.Runner (sydTestResult)
import System.Timeout (timeout)

main :: IO ()
main = sydTest spec

spec :: Spec
spec = describe "sydtest concurrency reproducer" $ do
  it "raises Unsatisfiable for an unbounded test function when the buffer is small" $ do
    let action =
          runWithOptions_ (\o -> o {runQuiet = True, runBufferSize = 8}) $ \tc ->
            let loop = do
                  _ <- choice tc 10
                  loop
             in loop
    action `shouldThrow` isUnsatisfiable

  it "satisfies preconditions when using assume" $ do
    runWithOptions_ (\o -> o {runQuiet = True}) $ \tc -> do
      n <- choice tc 9
      assume tc (n /= 0)
      n `shouldSatisfy` (/= 0)

  it "raises Unsatisfiable when all test cases are rejected" $ do
    let action =
          runWithOptions_ (\o -> o {runQuiet = True}) $ \tc -> do
            _ <- choice tc 9
            assume tc False
    action `shouldThrow` isUnsatisfiable

  it "does not deadlock under fail-fast even after many iterations" $ do
    let iterations = 50 :: Int
        timeoutMicros = 5 * 1000 * 1000
        settings :: Settings
        settings =
          defaultSettings
            { settingThreads = Asynchronous 4,
              settingFailFast = True
            }
        failingSpec :: Spec
        failingSpec =
          parallel $ do
            it "fails-1" $ do
              threadDelay 1000
              (expectationFailure "boom" :: IO ())
            it "fails-2" $ do
              threadDelay 1000
              (expectationFailure "boom" :: IO ())
            it "fails-3" $ do
              threadDelay 1000
              (expectationFailure "boom" :: IO ())
            it "fails-4" $ do
              threadDelay 1000
              (expectationFailure "boom" :: IO ())
    forM_ [1 .. iterations] $ \iteration -> do
      mTimed <- timeout timeoutMicros (sydTestResult settings failingSpec)
      case mTimed of
        Nothing -> expectationFailure $ "Async runner deadlocked under fail-fast (iteration " ++ show iteration ++ ")"
        Just _ -> pure ()

runWithOptions :: (RunOptions -> RunOptions) -> (TestCase -> IO ()) -> IO RunOptions
runWithOptions tweak action =
  runPropertyWithOptions (MP.withRunOptions tweak (MP.property action))

runWithOptions_ :: (RunOptions -> RunOptions) -> (TestCase -> IO ()) -> IO ()
runWithOptions_ tweak = void . runWithOptions tweak

runPropertyWithOptions :: MP.Property -> IO RunOptions
runPropertyWithOptions p = do
  opts <- resolveRunOptions (MP.applyPropertyOptions defaultRunOptions p)
  MP.runProperty opts p
  pure opts

isUnsatisfiable :: Unsatisfiable -> Bool
isUnsatisfiable _ = True

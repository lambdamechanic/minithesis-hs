module Minithesis
  ( Status (..),
    Frozen (..),
    StopTest (..),
    ValueError (..),
    TestCase,
    forChoices,
    newTestCase,
    choice,
    forcedChoice,
    markStatus,
    setStatus,
    getStatus,
    getChoices,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.IORef
import Data.Maybe (isJust)
import Data.Word (Word64)
import System.Random (StdGen, randomR)

-- | Represents the outcome of executing a test case.
-- Ordering matches the original Python IntEnum.
data Status
  = Overrun
  | Invalid
  | Valid
  | Interesting
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Exception raised when operations occur on a completed test case.
newtype Frozen
  = Frozen
  { frozenReason :: String
  }
  deriving (Show)

instance Exception Frozen

-- | Exception used internally to short circuit test execution.
newtype StopTest
  = StopTest
  { stopReason :: Status
  }
  deriving (Show)

instance Exception StopTest

-- | Exception raised when a choice bound is invalid.
newtype ValueError = ValueError String
  deriving (Eq, Show)

instance Exception ValueError

data TestCase
  = TestCase
  { tcPrefix :: [Word64],
    tcRandom :: Maybe (IORef StdGen),
    tcMaxSize :: Maybe Int,
    tcChoices :: IORef [Word64],
    tcStatus :: IORef (Maybe Status),
    tcPrintResults :: Bool,
    tcDepth :: IORef Int,
    tcTargetingScore :: IORef (Maybe Integer)
  }

-- | Construct a deterministic test case that replays the supplied choices.
forChoices :: [Word64] -> Bool -> IO TestCase
forChoices prefix =
  newTestCaseWith prefix Nothing (Just (length prefix))

-- | Construct a fresh test case backed by an RNG.
newTestCase :: StdGen -> Int -> Bool -> IO TestCase
newTestCase gen maxSize =
  newTestCaseWith [] (Just gen) (Just maxSize)

newTestCaseWith :: [Word64] -> Maybe StdGen -> Maybe Int -> Bool -> IO TestCase
newTestCaseWith prefix randomSource maxSize printResults = do
  randomRef <- traverse newIORef randomSource
  choicesRef <- newIORef []
  statusRef <- newIORef Nothing
  depthRef <- newIORef 0
  targetingRef <- newIORef Nothing
  pure
    TestCase
      { tcPrefix = prefix,
        tcRandom = randomRef,
        tcMaxSize = maxSize,
        tcChoices = choicesRef,
        tcStatus = statusRef,
        tcPrintResults = printResults,
        tcDepth = depthRef,
        tcTargetingScore = targetingRef
      }

-- | Make a bounded choice, respecting the recorded prefix if present.
choice :: TestCase -> Integer -> IO Word64
choice tc n = do
  result <- makeChoice tc n (randomBounded tc n)
  printIfNeeded tc $ "choice(" ++ show n ++ "): " ++ show result
  pure result

-- | Insert a predetermined choice into the test case.
forcedChoice :: TestCase -> Integer -> IO Word64
forcedChoice tc n = do
  prepareChoice tc n
  let value = fromInteger n
  appendChoice tc value
  pure value

-- | Mark the status of the test case and abort execution.
markStatus :: TestCase -> Status -> IO a
markStatus tc status = do
  statusRef <- readIORef (tcStatus tc)
  when (isJust statusRef) $ throwIO (Frozen "test case already has a status")
  writeIORef (tcStatus tc) (Just status)
  throwIO (StopTest status)

-- | Unsafely set the status without triggering StopTest.
setStatus :: TestCase -> Status -> IO ()
setStatus tc status = writeIORef (tcStatus tc) (Just status)

-- | Read the current status.
getStatus :: TestCase -> IO (Maybe Status)
getStatus tc = readIORef (tcStatus tc)

-- | Read the recorded choice sequence.
getChoices :: TestCase -> IO [Word64]
getChoices tc = readIORef (tcChoices tc)

maxWord64 :: Integer
maxWord64 = toInteger (maxBound :: Word64)

prepareChoice :: TestCase -> Integer -> IO ()
prepareChoice tc n = do
  validateBound n
  ensureMutable tc
  ensureCapacity tc

validateBound :: Integer -> IO ()
validateBound n
  | n < 0 = throwIO (ValueError $ "Invalid choice " ++ show n)
  | n > maxWord64 = throwIO (ValueError $ "Invalid choice " ++ show n)
  | otherwise = pure ()

ensureMutable :: TestCase -> IO ()
ensureMutable tc = do
  statusRef <- readIORef (tcStatus tc)
  when (isJust statusRef) $ throwIO (Frozen "test case is frozen")

ensureCapacity :: TestCase -> IO ()
ensureCapacity tc = do
  choices <- readIORef (tcChoices tc)
  case tcMaxSize tc of
    Just limit | length choices >= limit -> do
      _ <- markStatus tc Overrun
      pure ()
    _ -> pure ()

appendChoice :: TestCase -> Word64 -> IO ()
appendChoice tc value = modifyIORef' (tcChoices tc) (<> [value])

makeChoice :: TestCase -> Integer -> IO Word64 -> IO Word64
makeChoice tc n fallback = do
  prepareChoice tc n
  choices <- readIORef (tcChoices tc)
  let idx = length choices
  value <-
    if idx < length (tcPrefix tc)
      then pure (tcPrefix tc !! idx)
      else fallback
  appendChoice tc value
  when (toInteger value > n) $ do
    _ <- markStatus tc Invalid
    pure ()
  pure value

randomBounded :: TestCase -> Integer -> IO Word64
randomBounded tc n =
  case tcRandom tc of
    Nothing -> throwIO (ValueError "random source unavailable for this test case")
    Just ref -> do
      gen <- readIORef ref
      let upper :: Word64
          upper = fromInteger n
          (value, nextGen) = randomR (0, upper) gen
      writeIORef ref nextGen
      pure value

printIfNeeded :: TestCase -> String -> IO ()
printIfNeeded tc message = do
  depth <- readIORef (tcDepth tc)
  when (tcPrintResults tc && depth == 0) $ putStrLn message

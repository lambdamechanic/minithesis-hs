module Minithesis.TestCase
  ( Status (..),
    Frozen (..),
    StopTest (..),
    ValueError (..),
    Unsatisfiable (..),
    TestCase (..),
    maxWord64,
    forChoices,
    forChoicesWithPrinter,
    newTestCase,
    newTestCaseWith,
    choice,
    forcedChoice,
    reject,
    assume,
    markStatus,
    setStatus,
    getStatus,
    getChoices,
    target,
    printIfNeeded,
    finaliseStatus,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless, when)
import Data.IORef
import Data.Maybe (isJust)
import Data.Word (Word64)
import System.Random (StdGen, randomR)

-- | Represents the outcome of executing a test case.
--   Ordering matches the original Python IntEnum for parity.
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

-- | Raised when a property has no satisfying examples.
data Unsatisfiable = Unsatisfiable
  deriving (Eq, Show)

instance Exception Unsatisfiable

data TestCase
  = TestCase
  { tcPrefix :: [Word64],
    tcRandom :: Maybe (IORef StdGen),
    tcMaxSize :: Maybe Int,
    tcChoices :: IORef [Word64],
    tcBounds :: IORef [Integer],
    tcStatus :: IORef (Maybe Status),
    tcPrintResults :: Bool,
    tcPrinter :: String -> IO (),
    tcDepth :: IORef Int,
    tcTargetingScore :: IORef (Maybe Double)
  }

maxWord64 :: Integer
maxWord64 = toInteger (maxBound :: Word64)

forChoices :: [Word64] -> Bool -> IO TestCase
forChoices prefix printResults =
  forChoicesWithPrinter prefix printResults putStrLn

forChoicesWithPrinter :: [Word64] -> Bool -> (String -> IO ()) -> IO TestCase
forChoicesWithPrinter prefix =
  newTestCaseWith prefix Nothing (Just (length prefix))

newTestCase :: StdGen -> Int -> Bool -> IO TestCase
newTestCase gen maxSize printResults = do
  randomRef <- newIORef gen
  newTestCaseWith [] (Just randomRef) (Just maxSize) printResults putStrLn

newTestCaseWith :: [Word64] -> Maybe (IORef StdGen) -> Maybe Int -> Bool -> (String -> IO ()) -> IO TestCase
newTestCaseWith prefix randomRef maxSize printResults printer = do
  choicesRef <- newIORef []
  boundsRef <- newIORef []
  statusRef <- newIORef Nothing
  depthRef <- newIORef 0
  targetingRef <- newIORef Nothing
  pure
    TestCase
      { tcPrefix = prefix,
        tcRandom = randomRef,
        tcMaxSize = maxSize,
        tcChoices = choicesRef,
        tcBounds = boundsRef,
        tcStatus = statusRef,
        tcPrintResults = printResults,
        tcPrinter = printer,
        tcDepth = depthRef,
        tcTargetingScore = targetingRef
      }

choice :: TestCase -> Integer -> IO Word64
choice tc n = do
  result <- makeChoice tc n (randomBounded tc n)
  printIfNeeded tc $ "choice(" ++ show n ++ "): " ++ show result
  pure result

forcedChoice :: TestCase -> Integer -> IO Word64
forcedChoice tc n = do
  prepareChoice tc n
  let value = fromInteger n
  appendChoice tc value
  pure value

reject :: TestCase -> IO a
reject tc = markStatus tc Invalid

assume :: TestCase -> Bool -> IO ()
assume tc condition = unless condition $ do
  _ <- reject tc
  pure ()

markStatus :: TestCase -> Status -> IO a
markStatus tc status = do
  statusRef <- readIORef (tcStatus tc)
  when (isJust statusRef) $ throwIO (Frozen "test case already has a status")
  writeIORef (tcStatus tc) (Just status)
  throwIO (StopTest status)

setStatus :: TestCase -> Status -> IO ()
setStatus tc status = writeIORef (tcStatus tc) (Just status)

getStatus :: TestCase -> IO (Maybe Status)
getStatus tc = readIORef (tcStatus tc)

getChoices :: TestCase -> IO [Word64]
getChoices tc = readIORef (tcChoices tc)

target :: TestCase -> Double -> IO ()
target tc score = writeIORef (tcTargetingScore tc) (Just score)

printIfNeeded :: TestCase -> String -> IO ()
printIfNeeded tc message = do
  depth <- readIORef (tcDepth tc)
  when (tcPrintResults tc && depth == 0) $ tcPrinter tc message

finaliseStatus :: TestCase -> IO Status
finaliseStatus tc = do
  current <- readIORef (tcStatus tc)
  case current of
    Nothing -> do
      writeIORef (tcStatus tc) (Just Valid)
      pure Valid
    Just status -> pure status

-- Internal helpers ------------------------------------------------------

prepareChoice :: TestCase -> Integer -> IO ()
prepareChoice tc n = do
  validateBound n
  ensureMutable tc
  ensureCapacity tc
  modifyIORef' (tcBounds tc) (<> [n])

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

{-# LANGUAGE RankNTypes #-}

module Minithesis.TestCase
  ( Status (..),
    Frozen (..),
    StopTest (..),
    ValueError (..),
    Unsatisfiable (..),
    TestCase,
    withChoices,
    withChoicesWithPrinter,
    withNewTestCase,
    withNewTestCaseWith,
    choice,
    forcedChoice,
    reject,
    assume,
    markStatus,
    setStatus,
    getStatus,
    getChoices,
    getBounds,
    getTargetingScore,
    target,
    printIfNeeded,
    finaliseStatus,
    pushDepth,
    popDepth,
    weighted,
    maxWord64,
  )
where

import Control.Exception (Exception, finally, throwIO)
import Control.Monad (unless, when)
import Data.IORef
import Data.Maybe (isJust)
import Data.Word (Word64)
import System.Random (StdGen, randomR)

-- | Represents the outcome of executing a test case.
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

-- Internal representation -------------------------------------------------

data MutableTestCase
  = MutableTestCase
  { mtcPrefix :: [Word64],
    mtcRandom :: Maybe (IORef StdGen),
    mtcMaxSize :: Maybe Int,
    mtcChoices :: IORef [Word64],
    mtcBounds :: IORef [Integer],
    mtcStatus :: IORef (Maybe Status),
    mtcPrintResults :: Bool,
    mtcPrinter :: String -> IO (),
    mtcDepth :: IORef Int,
    mtcTargetingScore :: IORef (Maybe Double),
    mtcActive :: IORef Bool
  }

newtype TestCase = TestCase (forall r. (MutableTestCase -> IO r) -> IO r)

mkTestCase :: MutableTestCase -> TestCase
mkTestCase mtc = TestCase $ \use -> do
  ensureActive mtc
  use mtc

withMutableTestCase :: TestCase -> (MutableTestCase -> IO a) -> IO a
withMutableTestCase (TestCase k) = k

ensureActive :: MutableTestCase -> IO ()
ensureActive mtc = do
  active <- readIORef (mtcActive mtc)
  unless active $ throwIO (Frozen "test case is no longer active")

-- Construction ------------------------------------------------------------

withChoices :: [Word64] -> Bool -> (TestCase -> IO a) -> IO a
withChoices prefix printResults =
  withChoicesWithPrinter prefix printResults putStrLn

withChoicesWithPrinter :: [Word64] -> Bool -> (String -> IO ()) -> (TestCase -> IO a) -> IO a
withChoicesWithPrinter prefix =
  withNewTestCaseWith prefix Nothing (Just (length prefix))

withNewTestCase :: StdGen -> Int -> Bool -> (TestCase -> IO a) -> IO a
withNewTestCase gen maxSize printResults action = do
  randomRef <- newIORef gen
  withNewTestCaseWith [] (Just randomRef) (Just maxSize) printResults putStrLn action

withNewTestCaseWith :: [Word64] -> Maybe (IORef StdGen) -> Maybe Int -> Bool -> (String -> IO ()) -> (TestCase -> IO a) -> IO a
withNewTestCaseWith prefix randomRef maxSize printResults printer action = do
  choicesRef <- newIORef []
  boundsRef <- newIORef []
  statusRef <- newIORef Nothing
  depthRef <- newIORef 0
  targetingRef <- newIORef Nothing
  activeRef <- newIORef True
  let mtc =
        MutableTestCase
          { mtcPrefix = prefix,
            mtcRandom = randomRef,
            mtcMaxSize = maxSize,
            mtcChoices = choicesRef,
            mtcBounds = boundsRef,
            mtcStatus = statusRef,
            mtcPrintResults = printResults,
            mtcPrinter = printer,
            mtcDepth = depthRef,
            mtcTargetingScore = targetingRef,
            mtcActive = activeRef
          }
  action (mkTestCase mtc) `finally` writeIORef activeRef False

-- Operations --------------------------------------------------------------

choice :: TestCase -> Integer -> IO Word64
choice tc n =
  withMutableTestCase tc $ \mtc -> do
    result <- makeChoice mtc n (randomBounded mtc n)
    printIfNeededMutable mtc $ "choice(" ++ show n ++ "): " ++ show result
    pure result

forcedChoice :: TestCase -> Integer -> IO Word64
forcedChoice tc n =
  withMutableTestCase tc $ \mtc -> do
    prepareChoice mtc n
    let value = fromInteger n
    appendChoice mtc value
    pure value

reject :: TestCase -> IO a
reject tc = withMutableTestCase tc markInvalid
  where
    markInvalid mtc = markStatusMutable mtc Invalid

assume :: TestCase -> Bool -> IO ()
assume tc condition = unless condition $ do
  _ <- reject tc
  pure ()

markStatus :: TestCase -> Status -> IO a
markStatus tc status =
  withMutableTestCase tc $ \mtc -> markStatusMutable mtc status

setStatus :: TestCase -> Status -> IO ()
setStatus tc status =
  withMutableTestCase tc $ \mtc -> writeIORef (mtcStatus mtc) (Just status)

getStatus :: TestCase -> IO (Maybe Status)
getStatus tc =
  withMutableTestCase tc $ \mtc -> readIORef (mtcStatus mtc)

getChoices :: TestCase -> IO [Word64]
getChoices tc =
  withMutableTestCase tc $ \mtc -> readIORef (mtcChoices mtc)

getBounds :: TestCase -> IO [Integer]
getBounds tc =
  withMutableTestCase tc $ \mtc -> readIORef (mtcBounds mtc)

getTargetingScore :: TestCase -> IO (Maybe Double)
getTargetingScore tc =
  withMutableTestCase tc $ \mtc -> readIORef (mtcTargetingScore mtc)

target :: TestCase -> Double -> IO ()
target tc score =
  withMutableTestCase tc $ \mtc -> writeIORef (mtcTargetingScore mtc) (Just score)

printIfNeeded :: TestCase -> String -> IO ()
printIfNeeded tc message =
  withMutableTestCase tc $ \mtc -> printIfNeededMutable mtc message

finaliseStatus :: TestCase -> IO Status
finaliseStatus tc =
  withMutableTestCase tc finaliseMutable

weighted :: TestCase -> Double -> IO Bool
weighted tc p =
  withMutableTestCase tc $ \mtc ->
    case () of
      _
        | p <= 0 -> do
            printIfNeededMutable mtc $ "weighted(" ++ show p ++ "): False"
            pure False
        | p >= 1 -> do
            printIfNeededMutable mtc $ "weighted(" ++ show p ++ "): True"
            pure True
        | otherwise -> do
            res <- draw mtc
            printIfNeededMutable mtc $ "weighted(" ++ show p ++ "): " ++ show res
            pure res
  where
    draw mtc =
      case mtcRandom mtc of
        Nothing -> pure False
        Just ref -> do
          gen <- readIORef ref
          let (x, nextGen) = randomR (0.0, 1.0 :: Double) gen
              res = x < p
          writeIORef ref nextGen
          pure res

-- Internal helpers --------------------------------------------------------

makeChoice :: MutableTestCase -> Integer -> IO Word64 -> IO Word64
makeChoice mtc n fallback = do
  prepareChoice mtc n
  choices <- readIORef (mtcChoices mtc)
  let idx = length choices
  value <-
    if idx < length (mtcPrefix mtc)
      then pure (mtcPrefix mtc !! idx)
      else fallback
  appendChoice mtc value
  when (toInteger value > n) $ do
    _ <- markStatusMutable mtc Invalid
    pure ()
  pure value

prepareChoice :: MutableTestCase -> Integer -> IO ()
prepareChoice mtc n = do
  validateBound n
  ensureMutable mtc
  ensureCapacity mtc
  modifyIORef' (mtcBounds mtc) (<> [n])

validateBound :: Integer -> IO ()
validateBound n
  | n < 0 = throwIO (ValueError $ "Invalid choice " ++ show n)
  | n > maxWord64 = throwIO (ValueError $ "Invalid choice " ++ show n)
  | otherwise = pure ()

ensureMutable :: MutableTestCase -> IO ()
ensureMutable mtc = do
  ensureActive mtc
  statusRef <- readIORef (mtcStatus mtc)
  when (isJust statusRef) $ throwIO (Frozen "test case is frozen")

ensureCapacity :: MutableTestCase -> IO ()
ensureCapacity mtc = do
  choices <- readIORef (mtcChoices mtc)
  case mtcMaxSize mtc of
    Just limit | length choices >= limit -> do
      _ <- markStatusMutable mtc Overrun
      pure ()
    _ -> pure ()

appendChoice :: MutableTestCase -> Word64 -> IO ()
appendChoice mtc value = modifyIORef' (mtcChoices mtc) (<> [value])

randomBounded :: MutableTestCase -> Integer -> IO Word64
randomBounded mtc n =
  case mtcRandom mtc of
    Nothing -> throwIO (ValueError "random source unavailable for this test case")
    Just ref -> do
      gen <- readIORef ref
      let upper :: Word64
          upper = fromInteger n
          (value, nextGen) = randomR (0, upper) gen
      writeIORef ref nextGen
      pure value

markStatusMutable :: MutableTestCase -> Status -> IO a
markStatusMutable mtc status = do
  ensureActive mtc
  current <- readIORef (mtcStatus mtc)
  when (isJust current) $ throwIO (Frozen "test case already has a status")
  writeIORef (mtcStatus mtc) (Just status)
  throwIO (StopTest status)

printIfNeededMutable :: MutableTestCase -> String -> IO ()
printIfNeededMutable mtc message = do
  depth <- readIORef (mtcDepth mtc)
  when (mtcPrintResults mtc && depth == 0) $ mtcPrinter mtc message

finaliseMutable :: MutableTestCase -> IO Status
finaliseMutable mtc = do
  current <- readIORef (mtcStatus mtc)
  case current of
    Nothing -> do
      writeIORef (mtcStatus mtc) (Just Valid)
      pure Valid
    Just status -> pure status

maxWord64 :: Integer
maxWord64 = toInteger (maxBound :: Word64)

-- Local helpers for strategies -------------------------------------------

modifyDepth :: MutableTestCase -> (Int -> Int) -> IO ()
modifyDepth mtc = modifyIORef' (mtcDepth mtc)

pushDepth :: TestCase -> IO ()
pushDepth tc =
  withMutableTestCase tc $ \mtc -> modifyDepth mtc (+ 1)

popDepth :: TestCase -> IO ()
popDepth tc =
  withMutableTestCase tc $ \mtc -> modifyDepth mtc (\d -> d - 1)

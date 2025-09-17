module Minithesis
  ( Status (..),
    Frozen (..),
    StopTest (..),
    ValueError (..),
    Unsatisfiable (..),
    RunOptions (..),
    defaultRunOptions,
    runTest,
    weighted,
    -- Generators
    Strategy,
    any,
    integers,
    lists,
    tuples,
    just,
    nothing,
    satisfying,
    mixOf,
    TestCase,
    forChoices,
    newTestCase,
    choice,
    forcedChoice,
    reject,
    assume,
    markStatus,
    setStatus,
    getStatus,
    getChoices,
  )
where

import Control.Exception (Exception, throwIO, try)
import Control.Monad (replicateM, unless, when)
import Data.IORef
import Data.Maybe (fromMaybe, isJust)
import Data.Word (Word64)
import System.Random (StdGen, mkStdGen, newStdGen, randomR)
import Prelude hiding (any)

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
    tcStatus :: IORef (Maybe Status),
    tcPrintResults :: Bool,
    tcDepth :: IORef Int,
    tcTargetingScore :: IORef (Maybe Integer)
  }

data RunOptions
  = RunOptions
  { runMaxExamples :: Int,
    runQuiet :: Bool,
    runSeed :: Maybe Int,
    runBufferSize :: Int
  }
  deriving (Eq, Show)

defaultRunOptions :: RunOptions
defaultRunOptions =
  RunOptions
    { runMaxExamples = 100,
      runQuiet = False,
      runSeed = Nothing,
      runBufferSize = bufferSize
    }

data TestingState
  = TestingState
  { tsRandom :: IORef StdGen,
    tsOptions :: RunOptions,
    tsTestFunction :: TestCase -> IO (),
    tsValid :: IORef Int,
    tsCalls :: IORef Int
  }

bufferSize :: Int
bufferSize = 8 * 1024

callLimit :: RunOptions -> Int
callLimit opts = runMaxExamples opts * 10

runTest :: RunOptions -> (TestCase -> IO ()) -> IO ()
runTest opts userFunction = do
  initialGen <- maybe newStdGen (pure . mkStdGen) (runSeed opts)
  randomRef <- newIORef initialGen
  validRef <- newIORef 0
  callsRef <- newIORef 0
  let state =
        TestingState
          { tsRandom = randomRef,
            tsOptions = opts,
            tsTestFunction = userFunction,
            tsValid = validRef,
            tsCalls = callsRef
          }
  runGeneration state

runGeneration :: TestingState -> IO ()
runGeneration state = do
  valid <- readIORef (tsValid state)
  if valid >= runMaxExamples (tsOptions state)
    then pure ()
    else do
      calls <- readIORef (tsCalls state)
      when (calls >= callLimit (tsOptions state)) $ throwIO Unsatisfiable
      testCase <-
        newTestCaseWith
          []
          (Just (tsRandom state))
          (Just (runBufferSize (tsOptions state)))
          (not (runQuiet (tsOptions state)))
      _ <- executeTestCase state testCase
      runGeneration state

executeTestCase :: TestingState -> TestCase -> IO Status
executeTestCase state testCase = do
  modifyIORef' (tsCalls state) (+ 1)
  _ <- try (tsTestFunction state testCase) :: IO (Either StopTest ())
  status <- finaliseStatus testCase
  when (status >= Valid) $ modifyIORef' (tsValid state) (+ 1)
  pure status

finaliseStatus :: TestCase -> IO Status
finaliseStatus testCase = do
  current <- readIORef (tcStatus testCase)
  case current of
    Nothing -> do
      writeIORef (tcStatus testCase) (Just Valid)
      pure Valid
    Just status -> pure status

-- | Construct a deterministic test case that replays the supplied choices.
forChoices :: [Word64] -> Bool -> IO TestCase
forChoices prefix =
  newTestCaseWith prefix Nothing (Just (length prefix))

-- | Construct a fresh test case backed by an RNG.
newTestCase :: StdGen -> Int -> Bool -> IO TestCase
newTestCase gen maxSize printResults = do
  randomRef <- newIORef gen
  newTestCaseWith [] (Just randomRef) (Just maxSize) printResults

newTestCaseWith :: [Word64] -> Maybe (IORef StdGen) -> Maybe Int -> Bool -> IO TestCase
newTestCaseWith prefix randomRef maxSize printResults = do
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

-- | Reject the current test case as invalid.
reject :: TestCase -> IO a
reject tc = markStatus tc Invalid

-- | Abort the current test case when the precondition is false.
assume :: TestCase -> Bool -> IO ()
assume tc condition = unless condition $ do
  _ <- reject tc
  pure ()

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

-- | Draw a boolean that is True with the given probability in [0, 1].
-- If the test case is deterministic (no RNG), this returns False for 0 < p < 1.
weighted :: TestCase -> Double -> IO Bool
weighted tc p
  | p <= 0 = do
      printIfNeeded tc $ "weighted(" ++ show p ++ "): False"
      pure False
  | p >= 1 = do
      printIfNeeded tc $ "weighted(" ++ show p ++ "): True"
      pure True
  | otherwise =
      case tcRandom tc of
        Nothing -> do
          printIfNeeded tc $ "weighted(" ++ show p ++ "): False"
          pure False
        Just ref -> do
          gen <- readIORef ref
          let (x, nextGen) = randomR (0.0, 1.0 :: Double) gen
              res = x < p
          writeIORef ref nextGen
          printIfNeeded tc $ "weighted(" ++ show p ++ "): " ++ show res
          pure res

-- Generators

-- | Minimal strategy type for generator-based APIs.
newtype Strategy a = Strategy {runStrategy :: TestCase -> IO a}

instance Functor Strategy where
  fmap f (Strategy g) = Strategy (fmap f . g)

instance Applicative Strategy where
  pure = just
  Strategy ff <*> Strategy fa = Strategy $ \tc -> do
    f <- ff tc
    a <- fa tc
    pure (f a)

instance Monad Strategy where
  Strategy fa >>= k = Strategy $ \tc -> do
    a <- fa tc
    let Strategy fb = k a
    fb tc

-- | Run a strategy to produce a value.
any :: TestCase -> Strategy a -> IO a
any tc (Strategy f) = f tc

-- | Integer strategy drawing from inclusive bounds [lo, hi].
integers :: Integer -> Integer -> Strategy Integer
integers lo hi = Strategy $ \tc -> do
  when (hi < lo) $ throwIO (ValueError $ "Invalid integer bounds [" ++ show lo ++ "," ++ show hi ++ "]")
  let spanN = hi - lo
  v <- choice tc spanN
  pure (lo + toInteger v)

-- | List strategy with optional size bounds.
lists :: Strategy a -> Maybe Int -> Maybe Int -> Strategy [a]
lists (Strategy elemS) minSize maxSize = Strategy $ \tc -> do
  let lo = max 0 (fromMaybe 0 minSize)
      hi = max lo (fromMaybe (lo + 10) maxSize) -- default modest max
      spanN = fromIntegral (hi - lo) :: Integer
  k <- choice tc spanN
  let len = lo + fromIntegral k
  replicateM len (elemS tc)

-- | Pair strategy combining two strategies.
tuples :: Strategy a -> Strategy b -> Strategy (a, b)
tuples (Strategy fa) (Strategy fb) = Strategy $ \tc -> do
  a <- fa tc
  b <- fb tc
  pure (a, b)

-- | Constant strategy.
just :: a -> Strategy a
just x = Strategy $ \_ -> pure x

-- | Strategy that always rejects, forcing Unsatisfiable at the run level.
nothing :: Strategy a
nothing = Strategy $ \tc -> reject tc

-- | Choose from a non-empty list of strategies. Empty list rejects.
mixOf :: [Strategy a] -> Strategy a
mixOf [] = nothing
mixOf xs = Strategy $ \tc -> do
  let n = length xs
  i <- choice tc (toInteger (n - 1))
  let Strategy f = xs !! fromIntegral i
  f tc

-- | Filter a strategy with a predicate, rejecting values that do not satisfy it.
satisfying :: Strategy a -> (a -> Bool) -> Strategy a
satisfying (Strategy fa) p = Strategy $ \tc -> do
  a <- fa tc
  if p a then pure a else reject tc

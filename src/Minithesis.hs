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
    named,
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
    target,
  )
where

import Control.Exception (Exception, SomeException, throwIO, try)
import Control.Monad (forM_, replicateM, unless, when)
import Data.IORef
import qualified Data.List as L
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Word (Word64)
import qualified System.IO.Unsafe as Unsafe
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
    tcBounds :: IORef [Integer],
    tcStatus :: IORef (Maybe Status),
    tcPrintResults :: Bool,
    tcPrinter :: String -> IO (),
    tcDepth :: IORef Int,
    tcTargetingScore :: IORef (Maybe Double)
  }

data RunOptions
  = RunOptions
  { runMaxExamples :: Int,
    runQuiet :: Bool,
    runSeed :: Maybe Int,
    runBufferSize :: Int,
    runPrinter :: String -> IO ()
  }

defaultRunOptions :: RunOptions
defaultRunOptions =
  RunOptions
    { runMaxExamples = 100,
      runQuiet = False,
      runSeed = Nothing,
      runBufferSize = bufferSize,
      runPrinter = putStrLn
    }

data TestingState
  = TestingState
  { tsRandom :: IORef StdGen,
    tsOptions :: RunOptions,
    tsTestFunction :: TestCase -> IO (),
    tsValid :: IORef Int,
    tsCalls :: IORef Int,
    tsBestScore :: IORef (Maybe Double),
    tsBestChoices :: IORef [Word64],
    tsBestBounds :: IORef [Integer],
    tsQueue :: IORef [[Word64]],
    tsResult :: IORef (Maybe [Word64]),
    tsTrivial :: IORef Bool
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
  bestScoreRef <- newIORef Nothing
  bestChoicesRef <- newIORef []
  bestBoundsRef <- newIORef []
  queueRef <- newIORef []
  resultRef <- newIORef Nothing
  trivialRef <- newIORef False
  let state =
        TestingState
          { tsRandom = randomRef,
            tsOptions = opts,
            tsTestFunction = userFunction,
            tsValid = validRef,
            tsCalls = callsRef,
            tsBestScore = bestScoreRef,
            tsBestChoices = bestChoicesRef,
            tsBestBounds = bestBoundsRef,
            tsQueue = queueRef,
            tsResult = resultRef,
            tsTrivial = trivialRef
          }
  runGeneration state
  -- optional targeted optimisation step similar to Python's TestingState.target
  targetOptimisation state
  -- shrinking step if we found an interesting example
  shrinkResult state
  -- Post conditions: if no valid cases, unsatisfiable
  valid <- readIORef (tsValid state)
  when (valid == 0) $ throwIO Unsatisfiable
  -- If we found an interesting result, replay it to raise the user's error and print values
  mRes <- readIORef (tsResult state)
  case mRes of
    Nothing -> pure ()
    Just choices -> do
      tc <- forChoicesWithPrinter choices (not (runQuiet opts)) (runPrinter opts)
      -- Replay without catching user exceptions so they propagate
      userFunction tc

runGeneration :: TestingState -> IO ()
runGeneration state = do
  res <- readIORef (tsResult state)
  valid <- readIORef (tsValid state)
  calls <- readIORef (tsCalls state)
  trivial <- readIORef (tsTrivial state)
  let opts = tsOptions state
  if isJust res || valid >= runMaxExamples opts || calls >= callLimit opts || trivial
    then pure ()
    else do
      prefixQueue <- readIORef (tsQueue state)
      let (prefixToUse, restQ) = case prefixQueue of
            [] -> ([], [])
            (p : ps) -> (p, ps)
      writeIORef (tsQueue state) restQ
      -- Suppress printing during generation; only print on final replay.
      testCase <-
        newTestCaseWith
          prefixToUse
          (Just (tsRandom state))
          (Just (runBufferSize (tsOptions state)))
          False
          (runPrinter opts)
      _ <- executeTestCase state testCase
      runGeneration state

executeTestCase :: TestingState -> TestCase -> IO Status
executeTestCase state testCase = do
  modifyIORef' (tsCalls state) (+ 1)
  r <- try (tsTestFunction state testCase) :: IO (Either SomeException ())
  case r of
    Left _ -> do
      m <- getStatus testCase
      case m of
        Nothing -> do
          _ <- try (markStatus testCase Interesting) :: IO (Either StopTest a)
          pure ()
        Just _ -> pure ()
    Right _ -> pure ()
  status <- finaliseStatus testCase
  when (status >= Valid) $ modifyIORef' (tsValid state) (+ 1)
  -- If the test case was invalid or overrun with no choices, mark test as trivial
  when (status >= Invalid) $ do
    choices <- getChoices testCase
    when (null choices) (writeIORef (tsTrivial state) True)
  updateTargeting state testCase
  when (status == Interesting) $ do
    -- Record best interesting example by shortlex order
    mBest <- readIORef (tsResult state)
    choices <- getChoices testCase
    let better a b = sortKey a < sortKey b
    case mBest of
      Nothing -> writeIORef (tsResult state) (Just choices)
      Just prev -> when (better choices prev) (writeIORef (tsResult state) (Just choices))
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
forChoices prefix printResults =
  forChoicesWithPrinter prefix printResults putStrLn

forChoicesWithPrinter :: [Word64] -> Bool -> (String -> IO ()) -> IO TestCase
forChoicesWithPrinter prefix =
  newTestCaseWith prefix Nothing (Just (length prefix))

-- | Construct a fresh test case backed by an RNG.
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

printIfNeeded :: TestCase -> String -> IO ()
printIfNeeded tc message = do
  depth <- readIORef (tcDepth tc)
  when (tcPrintResults tc && depth == 0) $ tcPrinter tc message

-- | Record a targeting score for the current test case.
target :: TestCase -> Double -> IO ()
target tc score = writeIORef (tcTargetingScore tc) (Just score)

-- Internal: capture best score/choices and enqueue mutations
updateTargeting :: TestingState -> TestCase -> IO ()
updateTargeting state tc = do
  mScore <- readIORef (tcTargetingScore tc)
  case mScore of
    Nothing -> pure ()
    Just sc -> do
      best <- readIORef (tsBestScore state)
      choices <- readIORef (tcChoices tc)
      bounds <- readIORef (tcBounds tc)
      bestChoices <- readIORef (tsBestChoices state)
      let improve = maybe True (sc >) best
      when improve $ do
        writeIORef (tsBestScore state) (Just sc)
        writeIORef (tsBestChoices state) choices
        writeIORef (tsBestBounds state) bounds
        let len = min (length choices) (length bounds)
            genMut idx extreme =
              let pre = take len choices
                  bestPre j = if j < length bestChoices then bestChoices !! j else 0
                  replaced =
                    [ if j < idx
                        then bestPre j
                        else
                          if j == idx
                            then fromInteger extreme
                            else pre !! j
                    | j <- [0 .. len - 1]
                    ]
               in replaced
            extremes i = [bounds !! i, 0]
            muts = [genMut i e | i <- [0 .. len - 1], e <- extremes i]
        modifyIORef' (tsQueue state) (muts <>)

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

-- | Minimal strategy type with a name for printing.
data Strategy a = Strategy
  { runStrategy :: TestCase -> IO a,
    strategyName :: String,
    strategyShow :: Maybe (a -> String)
  }

instance Functor Strategy where
  fmap f (Strategy g n _) = Strategy (fmap f . g) ("map(" ++ n ++ ")") Nothing

instance Applicative Strategy where
  pure = just
  Strategy ff nf _ <*> Strategy fa na _ =
    Strategy
      ( \tc -> do
          f <- ff tc
          a <- fa tc
          pure (f a)
      )
      ("ap(" ++ nf ++ "," ++ na ++ ")")
      Nothing

instance Monad Strategy where
  Strategy fa na _ >>= k =
    Strategy
      ( \tc -> do
          a <- fa tc
          let Strategy fb _ _ = k a
          fb tc
      )
      ("bind(" ++ na ++ ")")
      Nothing

-- | Run a strategy to produce a value.
any :: TestCase -> Strategy a -> IO a
any tc (Strategy f name mShow) = do
  modifyIORef' (tcDepth tc) (+ 1)
  result <- f tc
  modifyIORef' (tcDepth tc) (\d -> d - 1)
  case mShow of
    Just sh -> printIfNeeded tc $ "any(" ++ name ++ "): " ++ sh result
    Nothing -> pure ()
  pure result

-- | Integer strategy drawing from inclusive bounds [lo, hi].
integers :: Integer -> Integer -> Strategy Integer
integers lo hi =
  Strategy
    ( \tc -> do
        when (hi < lo) $ throwIO (ValueError $ "Invalid integer bounds [" ++ show lo ++ "," ++ show hi ++ "]")
        let spanN = hi - lo
        v <- choice tc spanN
        pure (lo + toInteger v)
    )
    ("integers(" ++ show lo ++ ", " ++ show hi ++ ")")
    (Just show)

-- | List strategy with optional size bounds.
lists :: (Show a) => Strategy a -> Maybe Int -> Maybe Int -> Strategy [a]
lists (Strategy elemS name _) minSize maxSize =
  Strategy
    ( \tc -> do
        let lo = max 0 (fromMaybe 0 minSize)
            hi = max lo (fromMaybe (lo + 10) maxSize) -- default modest max
            spanN = fromIntegral (hi - lo) :: Integer
        k <- choice tc spanN
        let len = lo + fromIntegral k
        replicateM len (elemS tc)
    )
    ("lists(" ++ name ++ ")")
    (Just show)

-- | Pair strategy combining two strategies.
tuples :: Strategy a -> Strategy b -> Strategy (a, b)
tuples (Strategy fa na _) (Strategy fb nb _) =
  Strategy
    ( \tc -> do
        a <- fa tc
        b <- fb tc
        pure (a, b)
    )
    ("tuples(" ++ na ++ "," ++ nb ++ ")")
    Nothing

-- | Constant strategy.
just :: a -> Strategy a
just x = Strategy (\_ -> pure x) "just" Nothing

-- | Strategy that always rejects, forcing Unsatisfiable at the run level.
nothing :: Strategy a
nothing = Strategy reject "nothing" Nothing

-- | Choose from a non-empty list of strategies. Empty list rejects.
mixOf :: [Strategy a] -> Strategy a
mixOf [] = nothing
mixOf xs =
  Strategy
    ( \tc -> do
        let n = length xs
        i <- choice tc (toInteger (n - 1))
        let Strategy f _ _ = xs !! fromIntegral i
        f tc
    )
    ("mixOf [" ++ L.intercalate "," (map strategyName xs) ++ "]")
    Nothing

-- | Assign a name and display function to an existing strategy.
named :: String -> (a -> String) -> Strategy a -> Strategy a
named name sh (Strategy f _ _) = Strategy f name (Just sh)

-- | Filter a strategy with a predicate, rejecting values that do not satisfy it.
satisfying :: Strategy a -> (a -> Bool) -> Strategy a
satisfying (Strategy fa name mShow) p =
  Strategy
    ( \tc -> do
        a <- fa tc
        if p a then pure a else reject tc
    )
    ("satisfying(" ++ name ++ ")")
    mShow

-- Utilities

sortKey :: [Word64] -> (Int, [Word64])
sortKey xs = (length xs, xs)

-- Removed unused showEllipsis helper to avoid warnings.

-- Targeted optimisation pass (adapted, simplified)
targetOptimisation :: TestingState -> IO ()
targetOptimisation state = do
  let opts = tsOptions state
  mScore <- readIORef (tsBestScore state)
  mRes <- readIORef (tsResult state)
  when (isNothing mRes && isJust mScore) $ do
    let adjust choices i step = do
          let j = fromIntegral i
          let new = replaceAt choices j (fromIntegral (fromIntegral (choices !! j) + step))
          tc <- forChoicesWithPrinter new False (runPrinter opts)
          _ <- executeTestCase state tc
          st <- getStatus tc
          sc <- readIORef (tcTargetingScore tc)
          pure $ case (st, sc, mScore) of
            (Just Valid, Just scv, Just best) -> scv > best
            (Just Valid, Just scv, Nothing) -> scv >= 0
            _ -> False
    bestChoices <- readIORef (tsBestChoices state)
    forM_ [0 .. length bestChoices - 1] $ \i -> do
      _ <- adjust bestChoices i (1 :: Integer)
      _ <- adjust bestChoices i ((-1) :: Integer)
      pure ()

replaceAt :: [a] -> Int -> a -> [a]
replaceAt xs i v = take i xs ++ [v] ++ drop (i + 1) xs

-- Shrinker (subset sufficient for current tests)
shrinkResult :: TestingState -> IO ()
shrinkResult state = do
  -- Clear cross-test cached evaluations to avoid stale results influencing
  -- shrinking decisions for the current test function.
  writeIORef evalCache []
  mRes <- readIORef (tsResult state)
  case mRes of
    Nothing -> pure ()
    Just res0 -> do
      let consider cand = do
            st <- cachedEvaluate state cand
            pure (st == Interesting)
          deleteChunks xs = do
            let tryK k cur = do
                  go (length cur - k - 1) cur
                  where
                    go i best =
                      if i < 0
                        then pure best
                        else do
                          -- Attempt to delete a window of size k starting at i.
                          -- If we are deleting choices after the first index,
                          -- also reduce the first choice (commonly a size/length)
                          -- by up to k to keep structures like lists consistent.
                          let baseCand = take i best ++ drop (i + k) best
                              cand =
                                if i > 0 && not (null best)
                                  then
                                    let len0 = case best of
                                          [] -> 0
                                          (x : _) -> fromIntegral x
                                        dec = min k len0
                                        new0 = fromIntegral (len0 - dec)
                                     in replaceAt baseCand 0 new0
                                  else baseCand
                          ok <- if i + k <= length best then consider cand else pure False
                          go (i - 1) (if ok then cand else best)
            foldM' xs [8, 7, 6, 5, 4, 3, 2, 1] tryK
          sortWindows xs = do
            let tryK k cur = do
                  let go i best =
                        if i < 0
                          then pure best
                          else do
                            let seg = take k (drop i best)
                                cand = take i best ++ L.sort seg ++ drop (i + k) best
                            ok <- consider cand
                            go (i - 1) (if ok then cand else best)
                  go (length cur - k - 1) cur
            foldM' xs [8, 7 .. 2] tryK
          chooseIfTrue cand best = do
            ok <- consider cand
            pure (if ok then cand else best)
          redistributePairs xs = do
            let step k cur = do
                  let go i best =
                        if i + k >= length best || i < 0
                          then pure best
                          else do
                            let j = i + k
                                a = best !! i
                                b = best !! j
                                swapCand = replaceAt (replaceAt best i b) j a
                            best1 <- if a > b then chooseIfTrue swapCand best else pure best
                            let a' = best1 !! i
                                b' = best1 !! j
                            if a' > 0
                              then do
                                v <- binSearchDown 0 a' $ \v -> do
                                  let cand = replaceAt (replaceAt best1 i v) j (b' + (a' - v))
                                  consider cand
                                let cand = replaceAt (replaceAt best1 i v) j (b' + (a' - v))
                                chooseIfTrue cand best1 >>= go (i - 1)
                              else go (i - 1) best1
                  go (length cur - 1 - k) cur
            step 2 xs >>= step 1
          -- Minimize each coordinate independently while preserving Interesting,
          -- prioritizing earlier coordinates first for lexicographic minimality.
          minimizeEach xs = do
            let go i best =
                  if i >= length best
                    then pure best
                    else do
                      let hi = best !! i
                      v <- binSearchDown 0 hi $ \v -> consider (replaceAt best i v)
                      let cand = replaceAt best i v
                      best' <- chooseIfTrue cand best
                      go (i + 1) best'
            go 0 xs
          -- Prefer canonicalizing two-element additive pairs to lexicographic
          -- minimal form when possible, e.g. (2,999) -> (1,1000).
          normalizePair best =
            case best of
              a : b : _
                | a > 0 -> do
                    let cand = replaceAt (replaceAt best 0 1) 1 (b + (a - 1))
                    chooseIfTrue cand best
              _ -> pure best
          loop prev = do
            improved1 <- deleteChunks prev
            improved2 <- sortWindows improved1
            improved2a <- normalizePair improved2
            improved3 <- redistributePairs improved2a
            improved4 <- minimizeEach improved3
            if improved4 == prev then pure prev else loop improved4
      res <- loop res0
      writeIORef (tsResult state) (Just res)

-- Simple foldM specialized to this module to avoid extra imports
foldM' :: (Monad m) => a -> [b] -> (b -> a -> m a) -> m a
foldM' z [] _ = pure z
foldM' z (x : xs) f = do
  z' <- f x z
  foldM' z' xs f

-- Binary search down to a local minimal v in [lo, hi] where predicate True
binSearchDown :: (Integral a) => a -> a -> (a -> IO Bool) -> IO a
binSearchDown lo hi f = do
  okLo <- f lo
  if okLo
    then pure lo
    else go lo hi
  where
    go l h
      | l + 1 >= h = pure h
      | otherwise = do
          let mid = l + (h - l) `div` 2
          ok <- f mid
          if ok then go l mid else go mid h

-- Cached evaluator of a choice sequence against the current test function
{-# NOINLINE evalCache #-}
evalCache :: IORef [([Word64], Status)]
evalCache = Unsafe.unsafePerformIO (newIORef [])

cachedEvaluate :: TestingState -> [Word64] -> IO Status
cachedEvaluate state choices = do
  cache <- readIORef evalCache
  case lookup choices cache of
    Just st -> pure st
    Nothing -> do
      let printer = runPrinter (tsOptions state)
      tc <- forChoicesWithPrinter choices False printer
      -- Run and capture status; catch exceptions and mark interesting if needed
      r <- try (tsTestFunction state tc) :: IO (Either SomeException ())
      case r of
        Left _ -> do
          m <- getStatus tc
          case m of
            Nothing -> do
              _ <- try (markStatus tc Interesting) :: IO (Either StopTest a)
              pure ()
            Just _ -> pure ()
        Right _ -> pure ()
      st <- finaliseStatus tc
      modifyIORef' evalCache ((choices, st) :)
      pure st

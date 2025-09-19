module Minithesis.Runner
  ( RunOptions (..),
    defaultRunOptions,
    resolveRunOptions,
    resolveRunOptionsWith,
    runTest,
    weighted,
    TestDatabase (..),
    TestDatabaseKey,
    directoryDatabase,
  )
where

import Control.Exception (SomeException, throwIO, try)
import Control.Monad (when)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Data.List (foldl')
import qualified Data.List as L
import Data.Maybe (isJust, isNothing)
import Data.Word (Word64)
import Minithesis.TestCase
  ( Status (..),
    StopTest (..),
    TestCase (..),
    Unsatisfiable (..),
    finaliseStatus,
    forChoicesWithPrinter,
    getChoices,
    getStatus,
    markStatus,
    maxWord64,
    newTestCaseWith,
    printIfNeeded,
  )
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import qualified System.IO.Unsafe as Unsafe
import System.Random (StdGen, mkStdGen, newStdGen, randomR)
import Text.Read (readMaybe)
import Prelude hiding (any)

data RunOptions
  = RunOptions
  { runMaxExamples :: Int,
    runQuiet :: Bool,
    runSeed :: Maybe Int,
    runBufferSize :: Int,
    runPrinter :: String -> IO (),
    runDatabase :: Maybe TestDatabase,
    runDatabaseKey :: TestDatabaseKey
  }

type TestDatabaseKey = String

data TestDatabase = TestDatabase
  { dbLookup :: TestDatabaseKey -> IO (Maybe BS.ByteString),
    dbInsert :: TestDatabaseKey -> BS.ByteString -> IO (),
    dbDelete :: TestDatabaseKey -> IO ()
  }

directoryDatabase :: FilePath -> IO TestDatabase
directoryDatabase root = do
  createDirectoryIfMissing True root
  pure
    TestDatabase
      { dbLookup = \k -> do
          let path = root </> k
          exists <- doesFileExist path
          if exists then Just <$> BS.readFile path else pure Nothing,
        dbInsert = \k payload -> BS.writeFile (root </> k) payload,
        dbDelete = \k -> do
          let path = root </> k
          exists <- doesFileExist path
          when exists (removeFile path)
      }

defaultRunOptions :: RunOptions
defaultRunOptions =
  RunOptions
    { runMaxExamples = 100,
      runQuiet = False,
      runSeed = Nothing,
      runBufferSize = bufferSize,
      runPrinter = putStrLn,
      runDatabase = Nothing,
      runDatabaseKey = "default"
    }

-- | Apply environment overrides (e.g. 'MINITHESIS_MAX_EXAMPLES') to options.
resolveRunOptions :: RunOptions -> IO RunOptions
resolveRunOptions opts = do
  mMaxExamples <- lookupEnv "MINITHESIS_MAX_EXAMPLES"
  pure $ maybe opts applyMaxExamples mMaxExamples
  where
    applyMaxExamples raw =
      case readMaybe raw of
        Just n | n > 0 -> opts {runMaxExamples = n}
        _ -> opts

-- | Convenience helper that applies a tweak to 'defaultRunOptions' before
-- resolving environment overrides.
resolveRunOptionsWith :: (RunOptions -> RunOptions) -> IO RunOptions
resolveRunOptionsWith tweak = resolveRunOptions (tweak defaultRunOptions)

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
  loadFromDatabase state
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
  storeToDatabase opts mRes
  case mRes of
    Nothing -> pure ()
    Just choices -> do
      tc <- forChoicesWithPrinter choices (not (runQuiet opts)) (runPrinter opts)
      -- Replay without catching user exceptions so they propagate
      userFunction tc

loadFromDatabase :: TestingState -> IO ()
loadFromDatabase state =
  case runDatabase (tsOptions state) of
    Nothing -> pure ()
    Just db -> do
      let key = runDatabaseKey (tsOptions state)
      mPayload <- dbLookup db key
      case mPayload of
        Nothing -> pure ()
        Just payload -> do
          let replayChoices = decodeChoices payload
          tc <- forChoicesWithPrinter replayChoices False (const (pure ()))
          _ <- executeTestCase state tc
          pure ()

storeToDatabase :: RunOptions -> Maybe [Word64] -> IO ()
storeToDatabase opts mRes =
  case runDatabase opts of
    Nothing -> pure ()
    Just db ->
      let key = runDatabaseKey opts
       in case mRes of
            Nothing -> dbDelete db key
            Just choices -> dbInsert db key (encodeChoices choices)

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

runGeneration :: TestingState -> IO ()
runGeneration state = do
  res <- readIORef (tsResult state)
  valid <- readIORef (tsValid state)
  calls <- readIORef (tsCalls state)
  trivial <- readIORef (tsTrivial state)
  mBest <- readIORef (tsBestScore state)
  let opts = tsOptions state
      -- Reserve roughly half the generation budget for targeted hill-climbing
      -- so we do not exhaust the limit before trying to exploit the best score.
      halfLimit = runMaxExamples opts `div` 2
      stopForTarget = isJust mBest && valid > halfLimit
  if isJust res || valid >= runMaxExamples opts || calls >= callLimit opts || trivial || stopForTarget
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

updateTargeting :: TestingState -> TestCase -> IO ()
updateTargeting state tc = do
  mScore <- readIORef (tcTargetingScore tc)
  case mScore of
    Nothing -> pure ()
    Just sc -> do
      best <- readIORef (tsBestScore state)
      choices <- getChoices tc
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
               in [if j < idx then bestPre j else if j == idx then fromInteger extreme else pre !! j | j <- [0 .. len - 1]]
            extremes i = [bounds !! i, 0]
            muts = [genMut i e | i <- [0 .. len - 1], e <- extremes i]
        modifyIORef' (tsQueue state) (muts <>)

-- Utilities

sortKey :: [Word64] -> (Int, [Word64])
sortKey xs = (length xs, xs)

-- Removed unused showEllipsis helper to avoid warnings.

-- Targeted optimisation pass (adapted, simplified)
targetOptimisation :: TestingState -> IO ()
targetOptimisation state = do
  res <- readIORef (tsResult state)
  when (isNothing res) $ do
    mScore <- readIORef (tsBestScore state)
    bestChoices <- readIORef (tsBestChoices state)
    bestBounds <- readIORef (tsBestBounds state)
    case (mScore, bestChoices) of
      (Just score, _ : _) -> do
        let opts = tsOptions state
            shouldContinue :: IO Bool
            shouldContinue = do
              res' <- readIORef (tsResult state)
              trivial <- readIORef (tsTrivial state)
              valid <- readIORef (tsValid state)
              calls <- readIORef (tsCalls state)
              let limit = callLimit opts
              pure (not trivial && isNothing res' && valid < runMaxExamples opts && calls < limit)
            randomIndex :: Int -> Int -> IO Int
            randomIndex lo hi = do
              gen <- readIORef (tsRandom state)
              let (v, gen') = randomR (lo, hi) gen
              writeIORef (tsRandom state) gen'
              pure v
        scoreRef <- newIORef score
        choicesRef <- newIORef bestChoices
        boundsRef <- newIORef bestBounds
        let adjust :: Int -> Integer -> IO Bool
            adjust idx step = do
              continue <- shouldContinue
              if not continue
                then pure False
                else do
                  choices <- readIORef choicesRef
                  bounds <- readIORef boundsRef
                  if idx >= length choices
                    then pure False
                    else do
                      let current = toInteger (choices !! idx)
                          bound = if idx < length bounds then bounds !! idx else toInteger maxWord64
                          newVal = current + step
                      if newVal < 0 || newVal > bound
                        then pure False
                        else do
                          let candidate = replaceAt choices idx (fromInteger newVal)
                          tc <- forChoicesWithPrinter candidate False (runPrinter opts)
                          _ <- executeTestCase state tc
                          newScoreM <- readIORef (tsBestScore state)
                          case newScoreM of
                            Nothing -> pure False
                            Just newScore -> do
                              oldScore <- readIORef scoreRef
                              if newScore > oldScore
                                then do
                                  writeIORef scoreRef newScore
                                  newChoices <- readIORef (tsBestChoices state)
                                  newBounds <- readIORef (tsBestBounds state)
                                  writeIORef choicesRef newChoices
                                  writeIORef boundsRef newBounds
                                  pure True
                                else pure False
            tryDirections :: Int -> IO Integer
            tryDirections idx = do
              improvedPos <- adjust idx 1
              if improvedPos
                then pure (1 :: Integer)
                else do
                  improvedNeg <- adjust idx (-1)
                  pure (if improvedNeg then -1 else 0)
            expand :: Int -> Integer -> Integer -> IO Integer
            expand idx sign step = do
              continue <- shouldContinue
              if not continue
                then pure step
                else do
                  improved <- adjust idx (sign * toInteger step)
                  if improved
                    then expand idx sign (step * 2)
                    else pure step
            whileAdjust :: Int -> Integer -> Integer -> IO ()
            whileAdjust idx sign step = do
              continue <- shouldContinue
              when continue $ do
                improved <- adjust idx (sign * toInteger step)
                when improved (whileAdjust idx sign step)
            refine :: Int -> Integer -> Integer -> IO ()
            refine idx sign step
              | step <= 0 = pure ()
              | step == 1 = whileAdjust idx sign 1
              | otherwise = do
                  whileAdjust idx sign step
                  refine idx sign (step `div` 2)
            loop :: IO ()
            loop = do
              continue <- shouldContinue
              when continue $ do
                choices <- readIORef choicesRef
                bounds <- readIORef boundsRef
                let len = min (length choices) (length bounds)
                if len <= 0
                  then pure ()
                  else do
                    idx <- randomIndex 0 (len - 1)
                    sign <- tryDirections idx
                    when (sign /= 0) $ do
                      failing <- expand idx sign 1
                      refine idx sign failing
                    loop
        loop
      _ -> pure ()

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
            let tryK k cur =
                  let start = length cur - k - 1
                      go i best
                        | i < 0 = pure best
                        | i >= length best = go (i - 1) best
                        | i + k > length best = go (i - 1) best
                        | otherwise = do
                            let attempt = take i best ++ drop (i + k) best
                            ok <- consider attempt
                            if ok
                              then go i attempt
                              else
                                let prevIdx = i - 1
                                 in if prevIdx >= 0 && prevIdx < length attempt
                                      then do
                                        let prevVal = attempt !! prevIdx
                                        if prevVal > 0
                                          then do
                                            let attempt2 = replaceAt attempt prevIdx (prevVal - 1)
                                            ok2 <- consider attempt2
                                            if ok2
                                              then go i attempt2
                                              else go (i - 1) best
                                          else go (i - 1) best
                                      else go (i - 1) best
                   in go start cur
            foldM' xs [8, 7, 6, 5, 4, 3, 2, 1] tryK
          zeroBlocks xs = do
            let tryK k cur =
                  let go i best
                        | i < 0 = pure best
                        | otherwise = do
                            let cand = take i best ++ replicate k 0 ++ drop (i + k) best
                            ok <- consider cand
                            if ok
                              then go (i - k) cand
                              else go (i - 1) best
                   in go (length cur - k) cur
            foldM' xs [8, 7 .. 2] tryK
          minimizeEach xs =
            let go i best
                  | i < 0 = pure best
                  | otherwise = do
                      let hi = best !! i
                      v <- binSearchDown 0 hi $ \v -> consider (replaceAt best i v)
                      go (i - 1) (replaceAt best i v)
             in go (length xs - 1) xs
          sortWindows xs = do
            let tryK k cur =
                  let go i best
                        | i < 0 = pure best
                        | otherwise = do
                            let seg = take k (drop i best)
                                cand = take i best ++ L.sort seg ++ drop (i + k) best
                            ok <- consider cand
                            go (i - 1) (if ok then cand else best)
                   in go (length cur - k - 1) cur
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
          loop prev = do
            improved1 <- deleteChunks prev
            improved2 <- zeroBlocks improved1
            improved3 <- minimizeEach improved2
            improved4 <- sortWindows improved3
            improved5 <- redistributePairs improved4
            if improved5 == prev then pure prev else loop improved5
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
              _ <- try (markStatus tc Interesting) :: IO (Either StopTest ())
              pure ()
            Just _ -> pure ()
        Right _ -> pure ()
      st <- finaliseStatus tc
      modifyIORef' evalCache ((choices, st) :)
      pure st

encodeChoices :: [Word64] -> BS.ByteString
encodeChoices = BL.toStrict . Builder.toLazyByteString . foldMap Builder.word64BE

decodeChoices :: BS.ByteString -> [Word64]
decodeChoices bs
  | BS.null bs = []
  | BS.length bs < 8 = []
  | otherwise =
      let (chunk, rest) = BS.splitAt 8 bs
          word = foldl' (\acc b -> (acc `shiftL` 8) .|. fromIntegral b) 0 (BS.unpack chunk)
       in word : decodeChoices rest

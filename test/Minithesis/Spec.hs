module Minithesis.Spec (spec) where

import Control.Exception (Exception, throwIO, try)
import Control.Monad (forM_, replicateM, when)
import Data.Functor (void)
import Data.IORef
import Minithesis
import Test.Hspec
import Prelude hiding (any)

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
      runWithOptions_ (\o -> o {runQuiet = True}) $ \tc -> do
        _ <- choice tc (2 ^ (64 :: Integer) - 1)
        pure ()

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

  describe "weighted" $ do
    it "prints a top-level weighted" $ do
      logsRef <- newIORef []
      let collect msg = modifyIORef' logsRef (++ [msg])
      tc <- forChoicesWithPrinter [] True collect
      res <- weighted tc 0.5
      res `shouldBe` False
      readIORef logsRef `shouldReturn` ["weighted(0.5): False"]

    it "impossible weighted still allows later Failure" $ do
      let action =
            runWithOptions_ (\o -> o {runQuiet = True}) $ \tc -> do
              _ <- choice tc 1
              forM_ [1 .. (10 :: Int)] $ \_ -> do
                w <- weighted tc 0.0
                when w (throwIO Failure)
              v <- choice tc 1
              when (v == 1) (throwIO Failure)
      action `shouldThrow` isFailure
    it "weighted 1.0 is always True (guaranteed)" $ do
      let action =
            runWithOptions_ (\o -> o {runQuiet = True}) $ \tc -> do
              b <- weighted tc 1.0
              when b $ throwIO Failure
      action `shouldThrow` isFailure

  describe "limits" $ do
    it "does not exceed max_examples" $ do
      let runs = [1, 2, 5, 10]
      forM_ runs $ \mx -> do
        calls <- newIORef (0 :: Int)
        opts <- runWithOptions (\o -> o {runQuiet = True, runMaxExamples = mx}) $ \tc -> do
          _ <- choice tc 10000
          _ <- choice tc 10000
          s <- readIORef calls
          writeIORef calls (s + 1)
        readIORef calls `shouldReturn` runMaxExamples opts

  describe "TestCase.forcedChoice" $ do
    it "rejects bounds that exceed 64 bits" $ do
      tc <- forChoices [] False
      forcedChoice tc (2 ^ (64 :: Integer)) `shouldThrow` isValueError

  describe "generators" $ do
    it "size bounds on list" $ do
      runWithOptions_ (\o -> o {runQuiet = True}) $ \tc -> do
        ls <- any tc (lists (integers 0 10) (Just 1) (Just 3))
        length ls `shouldSatisfy` (\n -> n >= 1 && n <= 3)
    it "mapped possibility" $ do
      runWithOptions_ (\o -> o {runQuiet = True}) $ \tc -> do
        v <- any tc (fmap (* 2) (integers 0 5))
        v `shouldSatisfy` even
    it "selected possibility" $ do
      runWithOptions_ (\o -> o {runQuiet = True}) $ \tc -> do
        v <- any tc (satisfying (integers 0 5) even)
        v `shouldSatisfy` even
    it "bound possibility" $ do
      runWithOptions_ (\o -> o {runQuiet = True}) $ \tc -> do
        (m, n) <- any tc $ do
          m <- integers 0 5
          tuples (just m) (integers m (m + 10))
        m `shouldSatisfy` (\x -> x >= 0 && x <= 5)
        n `shouldSatisfy` (\x -> x >= m && x <= m + 10)
    it "tuples combines strategies" $ do
      runWithOptions_ (\o -> o {runQuiet = True}) $ \tc -> do
        (a, b) <- any tc (tuples (integers 0 10) (integers 0 10))
        a `shouldSatisfy` (\x -> x >= 0 && x <= 10)
        b `shouldSatisfy` (\x -> x >= 0 && x <= 10)
    it "just wraps a constant value" $ do
      runWithOptions_ (\o -> o {runQuiet = True}) $ \tc -> do
        v <- any tc (just (42 :: Integer))
        v `shouldBe` 42
    it "nothing fails when forced" $ do
      let action = runWithOptions_ (\o -> o {runQuiet = True, runMaxExamples = 5}) $ \tc -> do
            _ <- any tc nothing
            pure ()
      action `shouldThrow` isUnsatisfiable
    it "cannot witness empty mix_of" $ do
      let action = runWithOptions_ (\o -> o {runQuiet = True, runMaxExamples = 5}) $ \tc -> do
            _ <- any tc (mixOf ([] :: [Strategy Integer]))
            pure ()
      action `shouldThrow` isUnsatisfiable
    it "can draw mixture" $ do
      runWithOptions_ (\o -> o {runQuiet = True}) $ \tc -> do
        m <- any tc (mixOf [integers (-5) 0, integers 2 5])
        m `shouldSatisfy` (\x -> (x >= -5 && x <= 0) || (x >= 2 && x <= 5))
        m `shouldSatisfy` (/= (1 :: Integer))
  describe "targeting" $ do
    let drawTwo tc = do
          n <- choice tc 1000
          m <- choice tc 1000
          let s = toInteger n + toInteger m
          pure (n, m, s)
    it "can target a score upwards without failing" $ do
      maxScoreRef <- newIORef (0 :: Integer)
      runWithOptions_ (\o -> o {runQuiet = True, runMaxExamples = 1000, runSeed = Just 0}) $ \tc -> do
        (_, _, s) <- drawTwo tc
        target tc (fromIntegral s)
        modifyIORef' maxScoreRef (max s)
      readIORef maxScoreRef `shouldReturn` 2000
    it "can target a score downwards" $ do
      minScoreRef <- newIORef (100000 :: Integer)
      runWithOptions_ (\o -> o {runQuiet = True, runMaxExamples = 200}) $ \tc -> do
        (_, _, s) <- drawTwo tc
        target tc (negate (fromIntegral s))
        modifyIORef' minScoreRef (min s)
      readIORef minScoreRef `shouldReturn` 0
    it "can target a score upwards to interesting and prints choices" $ do
      let prop =
            withRunOptions (\o -> o {runQuiet = False, runMaxExamples = 1000}) . property $ \tc -> do
              (_, _, s) <- drawTwo tc
              target tc (fromIntegral s)
              when (s == 2000) (throwIO Failure)
      (linesOut, res) <- collectProperty prop
      case res of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Failure"
      drop (length linesOut - 2) linesOut
        `shouldBe` ["choice(1000): 1000", "choice(1000): 1000"]
    it "targeting when most do not benefit prints expected choices" $ do
      let big = 10000 :: Integer
      let prop =
            withRunOptions (\o -> o {runQuiet = False, runMaxExamples = 1000}) . property $ \tc -> do
              _ <- choice tc 1000
              _ <- choice tc 1000
              score <- choice tc big
              target tc (fromIntegral score)
              when (toInteger score == big) (throwIO Failure)
      (linesOut, res2) <- collectProperty prop
      case res2 of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Failure"
      drop (length linesOut - 3) linesOut
        `shouldBe` ["choice(1000): 0", "choice(1000): 0", "choice(" ++ show big ++ "): " ++ show big]

    it "finds a local maximum (PORTED)" $ do
      forM_ [0 .. (99 :: Int)] $ \seed -> do
        let action =
              runWithOptions_
                ( \o ->
                    o
                      { runQuiet = True,
                        runMaxExamples = 200,
                        runSeed = Just seed
                      }
                )
                $ \tc -> do
                  m <- choice tc 1000
                  n <- choice tc 1000
                  let dm = fromIntegral m - 500 :: Double
                      dn = fromIntegral n - 500 :: Double
                      score = negate (dm * dm + dn * dn)
                  target tc score
                  when (m == 500 && n == 500) (throwIO Failure)
        result <- tryFailure action
        case result of
          Left _ -> pure ()
          Right _ -> expectationFailure "expected Failure"

    it "target and reduce (PORTED)" $ do
      let prop =
            withRunOptions (\o -> o {runQuiet = False, runMaxExamples = 1000}) . property $ \tc -> do
              m <- choice tc 100000
              target tc (fromIntegral m)
              when (m > 99900) (throwIO Failure)
      (linesOut, res) <- collectProperty prop
      case res of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Failure"
      last linesOut `shouldBe` "choice(100000): 99901"

  describe "shrinking" $ do
    it "finds small list (port of test_finds_small_list)" $ do
      -- Expect the minimal failing list to be [1001] and printed via any(...)
      forM_ [0 .. (9 :: Int)] $ \seed -> do
        let prop =
              withRunOptions (\o -> o {runQuiet = False, runMaxExamples = 200, runSeed = Just seed}) . property $ \tc -> do
                ls <- any tc (lists (integers 0 10000) Nothing Nothing)
                let s = sum ls
                when (s > 1000) (throwIO Failure)
        (linesOut, res) <- collectProperty prop
        case res of
          Left _ -> pure ()
          Right _ -> expectationFailure "expected Failure"
        last linesOut `shouldBe` "any(lists(integers(0, 10000))): [1001]"

    it "finds small list even with bad lists (PORTED)" $ do
      -- Port of reference test_finds_small_list_even_with_bad_lists
      -- Define a monadic strategy that first draws a length [0..10]
      -- then draws that many integers in [0..10000]. This mimics the
      -- problematic pattern described in the reference test.
      let badList =
            named "bad_list" show $ do
              n <- integers 0 10
              let k = fromInteger n
              replicateM k (integers 0 10000)
      forM_ [0 .. (9 :: Int)] $ \seed -> do
        let prop =
              withRunOptions (\o -> o {runQuiet = False, runMaxExamples = 200, runSeed = Just seed}) . property $ \tc -> do
                ls <- any tc badList
                let s = sum ls
                when (s > 1000) (throwIO Failure)
        (linesOut, res) <- collectProperty prop
        case res of
          Left _ -> pure ()
          Right _ -> expectationFailure "expected Failure"
        last linesOut `shouldBe` "any(bad_list): [1001]"

    it "reduces additive pairs (PORTED)" $ do
      -- Port of reference test_reduces_additive_pairs
      let prop =
            withRunOptions (\o -> o {runQuiet = False, runMaxExamples = 10000}) . property $ \tc -> do
              m <- choice tc 1000
              n <- choice tc 1000
              when (m + n > 1000) (throwIO Failure)
      (linesOut, res) <- collectProperty prop
      case res of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected Failure"
      linesOut `shouldBe` ["choice(1000): 1", "choice(1000): 1000"]

  describe "caching" $ do
    it "matches Python's test_function_cache" $ do
      callsRef <- newIORef (0 :: Int)
      cache <- cachedTestFunction $ \tc -> do
        modifyIORef' callsRef (+ 1)
        v <- choice tc 1000
        when (v >= 200) (markStatus tc Interesting)
        w <- choice tc 1
        when (w == 0) (reject tc)
      let expectStatus choices =
            case choices of
              [1, 1] -> Valid
              [1] -> Overrun
              [1000] -> Interesting
              [1000, 1] -> Interesting
              _ -> error "unexpected choice sequence"
          sequences = [[1, 1], [1], [1000], [1000], [1000, 1]]
      forM_ sequences $ \bs -> do
        st <- cache bs
        st `shouldBe` expectStatus bs
      readIORef callsRef `shouldReturn` 2

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

tryFailure :: IO a -> IO (Either Failure a)
tryFailure = try

runPropertyWithOptions :: Property -> IO RunOptions
runPropertyWithOptions prop = do
  opts <- resolveRunOptions (applyPropertyOptions defaultRunOptions prop)
  runProperty opts prop
  pure opts

runWithOptions :: (RunOptions -> RunOptions) -> (TestCase -> IO ()) -> IO RunOptions
runWithOptions tweak action =
  runPropertyWithOptions (withRunOptions tweak (property action))

runWithOptions_ :: (RunOptions -> RunOptions) -> (TestCase -> IO ()) -> IO ()
runWithOptions_ tweak = void . runWithOptions tweak

collectProperty :: Property -> IO ([String], Either Failure ())
collectProperty prop = do
  ref <- newIORef []
  let capture msg = modifyIORef' ref (++ [msg])
  opts <- resolveRunOptions (applyPropertyOptions defaultRunOptions prop)
  let opts' = opts {runPrinter = capture}
  result <- tryFailure (runProperty opts' prop)
  logs <- readIORef ref
  pure (logs, result)

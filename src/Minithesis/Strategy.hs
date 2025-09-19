module Minithesis.Strategy
  ( Strategy (..),
    any,
    integers,
    lists,
    tuples,
    just,
    nothing,
    mixOf,
    satisfying,
    named,
  )
where

import Control.Exception (throwIO)
import Control.Monad (replicateM, when)
import Data.IORef (modifyIORef')
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Minithesis.TestCase
  ( TestCase (..),
    ValueError (..),
    choice,
    printIfNeeded,
    reject,
  )
import Prelude hiding (any)

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
            hi = max lo (fromMaybe (lo + 10) maxSize)
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

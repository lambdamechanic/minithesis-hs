module Minithesis.Cache
  ( cachedTestFunction,
  )
where

import Control.Exception (SomeException, try)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Minithesis.TestCase
  ( Status (..),
    StopTest,
    TestCase,
    finaliseStatus,
    getChoices,
    getStatus,
    markStatus,
    withChoicesWithPrinter,
  )

cachedTestFunction :: (TestCase -> IO ()) -> IO ([Word64] -> IO Status)
cachedTestFunction testFn = do
  cacheRef <- newIORef cacheEmpty
  pure $ \choices -> do
    cache <- readIORef cacheRef
    case cacheLookup cache choices of
      Just st -> pure st
      Nothing -> do
        (st, recorded) <-
          withChoicesWithPrinter choices False (const (pure ())) $ \tc -> do
            r <- try (testFn tc) :: IO (Either SomeException ())
            case r of
              Left _ -> do
                m <- getStatus tc
                case m of
                  Nothing -> do
                    _ <- try (markStatus tc Interesting) :: IO (Either StopTest ())
                    pure ()
                  Just _ -> pure ()
              Right _ -> pure ()
            status <- finaliseStatus tc
            recordedChoices <- getChoices tc
            pure (status, recordedChoices)
        modifyIORef' cacheRef (\tree -> cacheInsert tree recorded st)
        pure st

-- Internal trie ---------------------------------------------------------

data CacheTree
  = CacheBranch !(Map.Map Word64 CacheTree)
  | CacheLeaf !Status

cacheEmpty :: CacheTree
cacheEmpty = CacheBranch Map.empty

cacheLookup :: CacheTree -> [Word64] -> Maybe Status
cacheLookup (CacheLeaf status) _ = Just status
cacheLookup (CacheBranch _) [] = Just Overrun
cacheLookup (CacheBranch m) (c : cs) =
  case Map.lookup c m of
    Nothing -> Nothing
    Just child -> cacheLookup child cs

cacheInsert :: CacheTree -> [Word64] -> Status -> CacheTree
cacheInsert node [] status
  | status == Overrun = node
  | otherwise = CacheLeaf status
cacheInsert (CacheLeaf _) choices status = cacheInsert cacheEmpty choices status
cacheInsert (CacheBranch m) (c : cs) status
  | null cs && status /= Overrun = CacheBranch (Map.insert c (CacheLeaf status) m)
  | otherwise =
      let child = case Map.lookup c m of
            Just (CacheBranch m') -> CacheBranch m'
            _ -> cacheEmpty
          child' = cacheInsert child cs status
       in CacheBranch (Map.insert c child' m)

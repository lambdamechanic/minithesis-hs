{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Minithesis.GenericSpec (spec) where

import Control.Monad (when)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Minithesis (Strategy, any, integers, named, withChoices)
import Minithesis.Generic (HasStrategy (strategy))
import Minithesis.Sydtest (prop)
import Test.Syd hiding (prop)
import Prelude hiding (any)

newtype WrappedInt = WrappedInt Int
  deriving stock (Eq, Show, Generic)

instance HasStrategy WrappedInt

data Foo = Foo
  { fooNumber :: WrappedInt,
    fooFlag :: Maybe Bool
  }
  deriving stock (Eq, Show, Generic)

instance HasStrategy Foo

data Shape
  = Circle WrappedInt
  | Rectangle Bool Bool
  deriving stock (Eq, Show, Generic)

instance HasStrategy Shape

instance HasStrategy Int where
  strategy =
    let lo, hi :: Integer
        lo = -1000
        hi = 1000
     in named "int" show $
          fmap fromInteger (integers lo hi)

spec :: Spec
spec = do
  describe "genericStrategy for records" $ do
    it "draws a record using derived strategy including Just constructor" $ do
      let offsets :: [Word64]
          offsets =
            [ intOffset 7,
              1,
              0
            ]
      withChoices offsets False $ \tc -> do
        Foo number flag <- any tc (strategy :: Strategy Foo)
        number `shouldBe` WrappedInt 7
        flag `shouldBe` Just False

    it "draws a record using derived strategy including Nothing constructor" $ do
      let offsets :: [Word64]
          offsets =
            [ intOffset (-3),
              0
            ]
      withChoices offsets False $ \tc -> do
        Foo number flag <- any tc (strategy :: Strategy Foo)
        number `shouldBe` WrappedInt (-3)
        flag `shouldBe` Nothing

  describe "genericStrategy for sums" $ do
    it "selects the first constructor" $ do
      let offsets :: [Word64]
          offsets =
            [ 0,
              intOffset 5
            ]
      withChoices offsets False $ \tc -> do
        result <- any tc (strategy :: Strategy Shape)
        result `shouldBe` Circle (WrappedInt 5)

    it "selects the second constructor" $ do
      let offsets :: [Word64]
          offsets =
            [ 1,
              0,
              1
            ]
      withChoices offsets False $ \tc -> do
        result <- any tc (strategy :: Strategy Shape)
        result `shouldBe` Rectangle False True

  prop "derived strategy for Foo respects test-int bounds" $ \tc -> do
    Foo (WrappedInt n) _ <- any tc (strategy :: Strategy Foo)
    let lo = -1000
        hi = 1000
    when (n < lo || n > hi) $
      expectationFailure $
        "WrappedInt out of bounds: " <> show n

intOffset :: Int -> Word64
intOffset n =
  let lo = -1000
   in fromIntegral (n - lo)

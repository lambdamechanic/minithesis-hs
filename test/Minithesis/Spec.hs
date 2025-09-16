module Minithesis.Spec (spec) where

import Minithesis
import Test.Hspec

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
    it "satisfies preconditions when using assume" $ do
      let opts = defaultRunOptions {runQuiet = True}
      runTest opts $ \tc -> do
        n <- choice tc 9
        assume tc (n /= 0)
        n `shouldSatisfy` (/= 0)
isFrozen :: Frozen -> Bool
isFrozen _ = True

isValueError :: ValueError -> Bool
isValueError _ = True

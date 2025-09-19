module Minithesis.Tasty
  ( testProperty,
    testPropertyWith,
  )
where

import Minithesis.Property (ToProperty (..), applyPropertyOptions, runProperty)
import Minithesis.Runner (RunOptions, defaultRunOptions, resolveRunOptions)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

-- | Modelled after Test.Tasty.QuickCheck.testProperty
testProperty :: (ToProperty p) => String -> p -> TestTree
testProperty = testPropertyWith defaultRunOptions

-- | Variant that allows specifying Minithesis RunOptions.
testPropertyWith :: (ToProperty p) => RunOptions -> String -> p -> TestTree
testPropertyWith base name propertyLike =
  testCase name $ do
    let propValue = toProperty propertyLike
    opts <- resolveRunOptions (applyPropertyOptions base propValue)
    runProperty opts propValue

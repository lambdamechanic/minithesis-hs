module Minithesis.Tasty
  ( testProperty,
    testPropertyWith,
  )
where

import Minithesis (RunOptions, TestCase, defaultRunOptions, runTest)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

-- | Modelled after Test.Tasty.QuickCheck.testProperty
testProperty :: String -> (TestCase -> IO ()) -> TestTree
testProperty = testPropertyWith defaultRunOptions

-- | Variant that allows specifying Minithesis RunOptions.
testPropertyWith :: RunOptions -> String -> (TestCase -> IO ()) -> TestTree
testPropertyWith opts name property = testCase name (runTest opts property)

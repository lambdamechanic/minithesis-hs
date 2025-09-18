module Minithesis.Tasty
  ( minithesisTest,
    minithesisTestWith,
  )
where

import Minithesis (RunOptions, TestCase, defaultRunOptions, runTest)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

minithesisTest :: String -> (TestCase -> IO ()) -> TestTree
minithesisTest = minithesisTestWith defaultRunOptions

minithesisTestWith :: RunOptions -> String -> (TestCase -> IO ()) -> TestTree
minithesisTestWith opts name property = testCase name (runTest opts property)

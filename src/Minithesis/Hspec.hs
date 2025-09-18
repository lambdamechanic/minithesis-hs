module Minithesis.Hspec
  ( minithesis,
    minithesisWith,
  )
where

import Minithesis (RunOptions, TestCase, defaultRunOptions, runTest)
import Test.Hspec (Spec, it)

minithesis :: String -> (TestCase -> IO ()) -> Spec
minithesis = minithesisWith defaultRunOptions

minithesisWith :: RunOptions -> String -> (TestCase -> IO ()) -> Spec
minithesisWith opts name property = it name (runTest opts property)

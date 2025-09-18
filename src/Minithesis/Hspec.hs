module Minithesis.Hspec
  ( prop,
    propWith,
  )
where

import Minithesis (RunOptions, TestCase, defaultRunOptions, runTest)
import Test.Hspec (Spec, it)

-- | Modelled after Test.Hspec.QuickCheck.prop
prop :: String -> (TestCase -> IO ()) -> Spec
prop = propWith defaultRunOptions

-- | Variant that allows specifying Minithesis RunOptions.
propWith :: RunOptions -> String -> (TestCase -> IO ()) -> Spec
propWith opts name property = it name (runTest opts property)

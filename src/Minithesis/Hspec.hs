module Minithesis.Hspec
  ( prop,
    propWith,
  )
where

import Minithesis.Property (ToProperty (..), applyPropertyOptions, runProperty)
import Minithesis.Runner (RunOptions, defaultRunOptions, resolveRunOptions)
import Test.Hspec (Spec, it)

-- | Modelled after Test.Hspec.QuickCheck.prop
prop :: (ToProperty p) => String -> p -> Spec
prop = propWith defaultRunOptions

-- | Variant that allows specifying Minithesis RunOptions.
propWith :: (ToProperty p) => RunOptions -> String -> p -> Spec
propWith base name propertyLike =
  it name $ do
    let propValue = toProperty propertyLike
    opts <- resolveRunOptions (applyPropertyOptions base propValue)
    runProperty opts propValue

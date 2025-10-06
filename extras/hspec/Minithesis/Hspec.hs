module Minithesis.Hspec
  ( prop,
    propWith,
  )
where

import GHC.Stack (HasCallStack, withFrozenCallStack)
import Minithesis.Integration
  ( databaseDisabled,
    hashedLabel,
    scopedCallsite,
    scopedDatabase,
  )
import Minithesis.Property (ToProperty (..), applyPropertyOptions, runProperty)
import Minithesis.Runner
  ( RunOptions,
    defaultRunOptions,
    resolveRunOptions,
    runDatabase,
    runDatabaseKey,
  )
import Test.Hspec (Spec, it)

-- | Modelled after Test.Hspec.QuickCheck.prop
prop :: (HasCallStack, ToProperty p) => String -> p -> Spec
prop = propWith defaultRunOptions

-- | Variant that allows specifying Minithesis RunOptions.
propWith :: (HasCallStack, ToProperty p) => RunOptions -> String -> p -> Spec
propWith base name propertyLike =
  withFrozenCallStack $
    it name $ do
      let propValue = toProperty propertyLike
          callsite = scopedCallsite [".scopedCallsite", ".propWith", ".prop"]
          fixedKey = "hspec-" <> hashedLabel (name <> "@" <> callsite)
      opts0 <- resolveRunOptions (applyPropertyOptions base propValue)
      dbDisabled <- databaseDisabled
      opts <-
        if dbDisabled
          then pure opts0 {runDatabase = Nothing}
          else do
            db <- scopedDatabase fixedKey
            pure opts0 {runDatabase = Just db, runDatabaseKey = fixedKey}
      runProperty opts propValue
{-# ANN propWith ("HLint: ignore Redundant bracket" :: String) #-}

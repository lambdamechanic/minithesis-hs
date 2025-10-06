module Minithesis.Tasty
  ( testProperty,
    testPropertyWith,
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
import Minithesis
  ( RunOptions,
    defaultRunOptions,
    resolveRunOptions,
    runDatabase,
    runDatabaseKey,
  )
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase)

-- | Modelled after Test.Tasty.QuickCheck.testProperty
testProperty :: (HasCallStack, ToProperty p) => String -> p -> TestTree
testProperty = testPropertyWith defaultRunOptions

-- | Variant that allows specifying Minithesis RunOptions.
testPropertyWith :: (HasCallStack, ToProperty p) => RunOptions -> String -> p -> TestTree
testPropertyWith base name propertyLike =
  withFrozenCallStack $
    testCase name $ do
      let propValue = toProperty propertyLike
          callsite = scopedCallsite [".scopedCallsite", ".testPropertyWith", ".testProperty"]
          fixedKey = "tasty-" <> hashedLabel (name <> "@" <> callsite)
      opts0 <- resolveRunOptions (applyPropertyOptions base propValue)
      dbDisabled <- databaseDisabled
      opts <-
        if dbDisabled
          then pure opts0 {runDatabase = Nothing}
          else do
            db <- scopedDatabase fixedKey
            pure opts0 {runDatabase = Just db, runDatabaseKey = fixedKey}
      runProperty opts propValue
{-# ANN testPropertyWith ("HLint: ignore Redundant bracket" :: String) #-}

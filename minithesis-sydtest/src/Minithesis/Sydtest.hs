module Minithesis.Sydtest
  ( prop,
    propWith,
  )
where

import GHC.Stack (HasCallStack, withFrozenCallStack)
import Minithesis
  ( RunOptions,
    defaultRunOptions,
    resolveRunOptions,
    runDatabase,
    runDatabaseKey,
  )
import Minithesis.Integration
  ( databaseDisabled,
    hashedLabel,
    scopedCallsite,
    scopedDatabase,
  )
import Minithesis.Property (ToProperty (..), applyPropertyOptions, runProperty)
import Test.Syd (Spec, it)

-- | Register a Minithesis property as a Sydtest example.
prop :: (HasCallStack, ToProperty p) => String -> p -> Spec
prop = propWith defaultRunOptions

-- | Variant that allows specifying Minithesis RunOptions.
propWith :: (HasCallStack, ToProperty p) => RunOptions -> String -> p -> Spec
propWith base name propertyLike =
  withFrozenCallStack $
    it name $ do
      let propValue = toProperty propertyLike
          callsite = scopedCallsite [".scopedCallsite", ".propWith", ".prop"]
          fixedKey = "sydtest-" <> hashedLabel (name <> "@" <> callsite)
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

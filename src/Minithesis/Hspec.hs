module Minithesis.Hspec
  ( prop,
    propWith,
  )
where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray.Encoding (Base (Base64URLUnpadded), convertToBase)
import qualified Data.ByteString.Char8 as BS
import Data.List (isSuffixOf)
import GHC.Stack (HasCallStack, SrcLoc (..), callStack, getCallStack, withFrozenCallStack)
import Minithesis.Property (ToProperty (..), applyPropertyOptions, runProperty)
import Minithesis.Runner
  ( RunOptions,
    TestDatabase (..),
    defaultRunOptions,
    directoryDatabase,
    resolveRunOptions,
    runDatabase,
    runDatabaseKey,
  )
import System.Environment (lookupEnv)
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
          callsite = scopedCallsite
          fixedKey = "hspec-" <> hashed (name <> "@" <> callsite)
      opts0 <- resolveRunOptions (applyPropertyOptions base propValue)
      dbDisabled <- databaseDisabled
      opts <-
        if dbDisabled
          then pure opts0 {runDatabase = Nothing}
          else do
            db <- scopedDatabase fixedKey
            pure opts0 {runDatabase = Just db, runDatabaseKey = fixedKey}
      runProperty opts propValue

databaseDisabled :: IO Bool
databaseDisabled = do
  m <- lookupEnv "MINITHESIS_NO_DATABASE"
  pure (maybe False (not . null) m)

scopedDatabase :: String -> IO TestDatabase
scopedDatabase fixedKey = do
  base <- directoryDatabase ".minithesis-hs-db"
  pure
    TestDatabase
      { dbLookup = \_ -> dbLookup base fixedKey,
        dbInsert = \_ payload -> dbInsert base fixedKey payload,
        dbDelete = \_ -> dbDelete base fixedKey
      }

hashed :: String -> String
hashed label = BS.unpack $ convertToBase Base64URLUnpadded digest
  where
    digest :: Digest SHA256
    digest = hash (BS.pack label)

{-# ANN scopedCallsite ("HLint: ignore Redundant bracket" :: String) #-}
scopedCallsite :: (HasCallStack) => String
scopedCallsite =
  case dropWhile (isInternal . fst) (getCallStack callStack) of
    [] -> "unknown"
    (name, src) : _ -> render name src
  where
    isInternal name = any (`isSuffixOf` name) [".scopedCallsite", ".propWith", ".prop"]
    render name src = srcLocFile src <> ":" <> show (srcLocStartLine src) <> "#" <> name

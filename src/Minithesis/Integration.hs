module Minithesis.Integration
  ( databaseDisabled,
    scopedDatabase,
    hashedLabel,
    scopedCallsite,
  )
where

import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray.Encoding (Base (Base64URLUnpadded), convertToBase)
import qualified Data.ByteString.Char8 as BS
import Data.List (isSuffixOf)
import GHC.Stack (HasCallStack, SrcLoc (..), callStack, getCallStack)
import Minithesis.Runner (TestDatabase (..), directoryDatabase)
import System.Environment (lookupEnv)

-- | Respect the integration escape hatch for disabling the failure database.
databaseDisabled :: IO Bool
databaseDisabled = do
  m <- lookupEnv "MINITHESIS_NO_DATABASE"
  pure (maybe False (not . null) m)

-- | Scope the database by prefixing all operations with the provided key.
scopedDatabase :: String -> IO TestDatabase
scopedDatabase fixedKey = do
  base <- directoryDatabase ".minithesis-hs-db"
  pure
    TestDatabase
      { dbLookup = \_ -> dbLookup base fixedKey,
        dbInsert = \_ payload -> dbInsert base fixedKey payload,
        dbDelete = \_ -> dbDelete base fixedKey
      }

-- | Produce a stable, file-system friendly hash of a label.
hashedLabel :: String -> String
hashedLabel label = BS.unpack $ convertToBase Base64URLUnpadded digest
  where
    digest :: Digest SHA256
    digest = hash (BS.pack label)

-- | Grab the first non-internal callsite from the call stack.
-- Ignore any entries whose rendered name ends with one of the supplied suffixes.
scopedCallsite :: (HasCallStack) => [String] -> String
scopedCallsite ignoreSuffixes =
  case dropWhile (isInternal . fst) (getCallStack callStack) of
    [] -> "unknown"
    (name, src) : _ -> render name src
  where
    isInternal name = any (`isSuffixOf` name) ignoreSuffixes
    render name src = srcLocFile src <> ":" <> show (srcLocStartLine src) <> "#" <> name

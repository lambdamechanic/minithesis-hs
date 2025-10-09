module Main (main) where

import qualified Minithesis.GenericSpec as GenericSpec
import Test.Syd (sydTest)

main :: IO ()
main = sydTest GenericSpec.spec

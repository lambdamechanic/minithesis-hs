module Main (main) where

import qualified Minithesis.Spec as Spec
import Test.Hspec (hspec)

main :: IO ()
main = hspec Spec.spec

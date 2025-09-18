module Main (main) where

import qualified Minithesis.Spec as Spec
import Test.Tasty (defaultMain)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = do
  tree <- testSpec "minithesis" Spec.spec
  defaultMain tree

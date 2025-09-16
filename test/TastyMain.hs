module Main (main) where

import qualified Minithesis.Spec as Spec
import Test.Tasty (defaultMain)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = do
  hspecTests <- testSpec "Minithesis" Spec.spec
  defaultMain hspecTests

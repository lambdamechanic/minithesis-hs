module Main (main) where

import qualified Minithesis.Spec as Spec
import Test.Syd (sydTest)

main :: IO ()
main = sydTest Spec.spec

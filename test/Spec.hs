module Main (main) where

import qualified Minithesis.Spec as Spec
import Test.Syd (sydTest)
import Test.Syd.Hspec (fromHspec)

main :: IO ()
main = sydTest (fromHspec Spec.spec)

module Main (main) where

import qualified Examples.HspecSpec as Examples
import Test.Hspec (hspec)

main :: IO ()
main = hspec Examples.spec

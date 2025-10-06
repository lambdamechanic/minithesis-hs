module Main (main) where

import qualified Examples.SydtestSpec as Examples
import Test.Syd

main :: IO ()
main = sydTest Examples.spec

module Main (main) where

import qualified Examples.SydtestSpec as Examples
import qualified Selectorsleuth.PropertySpec as Selectorsleuth
import Test.Syd

main :: IO ()
main =
  sydTest $ do
    Examples.spec
    Selectorsleuth.spec

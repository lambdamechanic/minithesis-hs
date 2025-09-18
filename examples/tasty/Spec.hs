module Main (main) where

import Data.List (sort)
import Minithesis
import Minithesis.Tasty (testProperty)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=))
import Prelude hiding (any)

tests :: TestTree
tests =
  testGroup
    "Examples with Tasty interface"
    [ testProperty "sorting twice equals sorting once" $ \tc -> do
        xs <- any tc (lists (integers (-10) 10) (Just 0) (Just 20))
        sort (sort xs) @?= sort xs
    ]

main :: IO ()
main = defaultMain tests

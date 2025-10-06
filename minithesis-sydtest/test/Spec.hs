module Main (main) where

import Data.List (sort)
import Minithesis
import qualified Minithesis.Sydtest as MS
import Test.Syd
import Prelude hiding (any)

spec :: Spec
spec =
  describe "Sydtest adapter" $ do
    MS.prop "sorting twice equals sorting once" $
      withTests 50 $ \tc -> do
        xs <- any tc $ lists (integers (-5) 5) (Just 0) (Just 10)
        sort (sort xs) `shouldBe` sort xs

main :: IO ()
main = sydTest spec

module Examples.HspecSpec (spec) where

import Data.List (nub, sort)
import Minithesis
import qualified Minithesis.Hspec as MH
import Test.Hspec
import Prelude hiding (any)

spec :: Spec
spec = do
  describe "Examples with Hspec interface" $ do
    MH.prop "sorting twice equals sorting once" $
      withTests 200 $ \tc -> do
        xs <- any tc $ lists (integers (-10) 10) (Just 0) (Just 20)
        sort (sort xs) `shouldBe` sort xs
    MH.prop "sorting a uniqued list twice equals sorting once: should fail" $
      withTests 200 $ \tc -> do
        xs <- any tc $ lists (integers (-10) 10) (Just 0) (Just 20)
        sort (sort xs) `shouldBe` sort (nub xs)

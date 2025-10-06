module Examples.SydtestSpec (spec) where

import Data.List (sort)
import Minithesis
import qualified Minithesis.Sydtest as MS
import Test.Syd
import Prelude hiding (any)

spec :: Spec
spec =
  describe "Examples with Sydtest interface" $ do
    MS.prop "sorting twice equals sorting once" $
      withTests 200 $ \tc -> do
        xs <- any tc $ lists (integers (-10) 10) (Just 0) (Just 20)
        sort (sort xs) `shouldBe` sort xs

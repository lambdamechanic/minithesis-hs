module Examples.HspecSpec (spec) where

import Data.List (sort)
import Minithesis
import qualified Minithesis.Hspec as MH
import Test.Hspec
import Prelude hiding (any)

spec :: Spec
spec = do
  describe "Examples with Hspec interface" $ do
    MH.prop "sorting twice equals sorting once" $ \tc -> do
      xs <- any tc (lists (integers (-10) 10) (Just 0) (Just 20))
      sort (sort xs) `shouldBe` sort xs

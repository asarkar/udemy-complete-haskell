module Section16Spec (spec) where

import Section16 (acyclicPaths)
import Test.Hspec

spec :: Spec
spec = do
  describe "acyclicPaths" $ do
    it "returns all ayclic paths between the source and destination" $ do
      let edges = [(1, 2), (2, 3), (1, 3), (3, 4), (4, 2), (5, 6)]
      acyclicPaths 1 4 edges `shouldBe` [[1 :: Int, 2, 3, 4], [1, 3, 4]]

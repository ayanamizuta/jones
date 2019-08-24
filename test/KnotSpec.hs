module KnotSpec (spec) where

import Test.Hspec
import Knot

spec :: Spec
spec = do
  describe "connection_change" $
    it "connection_change 1" $
      let i1   = [InterSection {edges = (Front {tagf = 1},Front {tagf = 0},Back {tagb = 1},Back {tagb = 0})}]
          ians = [Trivial] in
      connection_change (tail i1) (Front 1,Back 0) (Front 0,Back 1) `shouldBe` ians

  describe "connection_change" $
    it "connection_change 2" $
      let i1   = [InterSection {edges = (Front {tagf = 1},Front {tagf = 0},Back {tagb = 1},Back {tagb = 0})}]
          ians = [Trivial,Trivial] in
      connection_change (tail i1) (Front 1,Back 1) (Front 0,Back 0) `shouldBe` ians

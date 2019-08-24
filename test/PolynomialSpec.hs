module PolynomialSpec (spec) where

import Test.Hspec
import Polynomial

spec :: Spec
spec = do
  describe "(+.)" $
    it "足し算" $
      [(-1,2),(2,5)] +. [(2,-4),(3,8)] `shouldBe` [(-1,2),(2,1),(3,8)]

  describe "(-.)" $
    it "引き算" $
      [(-1,2),(2,5)] -. [(2,-4),(3,8)] `shouldBe` [(-1,2),(2,9),(3,-8)]

  describe "(*.)" $
    it "掛け算" $
      [(-1,2),(2,5)] *. [(2,-4),(5,8)] `shouldBe` [(1,-8),(4,-4),(7,40)]

  describe "(+.),(-.),(*.)" $
    it "係数0のMonomial ない" $
      [(-2,3),(1,3)]*.([(-2,3),(1,3)]+.[(-2,3),(1,3)])-.[(-2,3),(1,3)] `shouldSatisfy` ((==0) . length . filter (==0) . map snd)

  describe "lp_to_string 1" $
    it "to Mathjax" $
      lp_to_string [(-1,2),(0,5),(2,1)] `shouldBe` "2A^{-1}+5+A^{2}"

  describe "lp_to_string 2" $
    it "to Mathjax" $
      lp_to_string [(0,1)] `shouldBe` "1"

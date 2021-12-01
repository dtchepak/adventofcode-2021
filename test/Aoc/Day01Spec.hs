module Aoc.Day01Spec (spec) where

import Test.Hspec
import Aoc.Day01 (increases)

spec :: Spec
spec = do
  describe "example data" $ do
    it "part 1: should find 1721 and 299" $ do
      let example = [ 199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
      increases example `shouldBe` 7
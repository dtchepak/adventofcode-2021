module Aoc.Day01Spec (spec) where

import Test.Hspec
import Aoc.Day01 (increases, window3, part2)

spec :: Spec
spec = do
  describe "example data" $ do
    let example = [ 199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
    it "part 1: should find 1721 and 299" $
      increases example `shouldBe` 7
    it "part 2: windows" $
      window3 example `shouldBe` [
        (199, 200, 208), -- A
        (200, 208, 210), -- B
        (208, 210, 200), -- C
        (210, 200, 207), -- D
        (200, 207, 240), -- E
        (207, 240, 269), -- F
        (240, 269, 260), -- G
        (269, 260, 263) -- H
      ]
    it "part 2: example" $
      part2 example `shouldBe` 5

{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day03Spec (spec) where

import Test.Hspec
import Aoc.Day03

spec :: Spec
spec = do
  describe "example data" $ do
    let example = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]
    it "part 1: gamma" $
      gamma example `shouldBe` "10110"
    it "part 1: binToInt" $
      binToInt "10110" `shouldBe` 22
    it "part 2: epsilon" $
      gammaToEpsilon "10110" `shouldBe` "01001"
{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day02Spec (spec) where

import Test.Hspec
import Aoc.Day02

spec :: Spec
spec = do
  describe "example data" $ do
    let sampleCommands = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"
    it "part 1: command parsing" $
      parseCommands sampleCommands
        `shouldBe` pure [Command Forward 5, Command Down 5, Command Forward 8, Command Up 3, Command Down 8, Command Forward 2]
    it "part 1: run" $
      (run <$> parseCommands sampleCommands) `shouldBe` pure (Position 15 10 0)
    it "part 2: forward 5" $
      run2 [Command Forward 5] `shouldBe` Position 5 0 0
    it "part 2: forward 5, down 5, forward 8" $
      run2 [Command Forward 5, Command Down 5, Command Forward 8] `shouldBe` Position 13 40 5
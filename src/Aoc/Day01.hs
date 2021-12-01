module Aoc.Day01 (increases, part1) where

increases :: [Int] -> Int
increases a = sum (zipWith (\x y -> if y > x then 1 else 0) a (drop 1 a))

part1 :: IO ()
part1 = input >>= print . increases

input :: IO [Int]
input =
  map read . lines  <$> readFile "data/day01.txt"
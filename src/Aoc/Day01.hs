module Aoc.Day01 (increases, window3, part1, part2, input) where

increases :: [Int] -> Int
increases a = sum (zipWith (\x y -> if y > x then 1 else 0) a (drop 1 a))

part1 :: IO ()
part1 = input >>= print . increases

window3 :: [Int] -> [(Int, Int, Int)]
window3 x =
  zip3 x (drop 1 x) (drop 2 x)

part2 :: [Int] -> Int
part2 =
  let sumTriple (x,y,z) = x + y + z
  in increases . map sumTriple . window3

input :: IO [Int]
input =
  map read . lines  <$> readFile "data/day01.txt"
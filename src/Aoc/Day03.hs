module Aoc.Day03 (
  gamma,
  binToInt,
  gammaToEpsilon,
  part1
) where

import Data.List (foldl')

data PositionAcc = PositionAcc { zeroCount :: Int, oneCount :: Int }
newtype Acc = Acc [PositionAcc]

instance Semigroup PositionAcc where
  (PositionAcc z o) <> (PositionAcc z' o') = PositionAcc (z+z') (o+o')

instance Monoid PositionAcc where
  mempty = PositionAcc 0 0

instance Semigroup Acc where
  Acc [] <> Acc p = Acc p
  Acc p <> Acc [] = Acc p
  Acc p <> Acc p' = Acc (zipWith (<>) p p')

instance Monoid Acc where
  mempty = Acc []

acc :: String -> Acc
acc = Acc . map (\c -> if c == '0' then PositionAcc 1 0 else PositionAcc 0 1)

unAcc :: Acc -> String
unAcc (Acc ps) = map (\(PositionAcc z o) -> if z > o then '0' else '1') ps

gamma :: [String] -> String
gamma = unAcc . foldMap acc

gammaToEpsilon :: String -> String
gammaToEpsilon = map (\c -> if c=='1' then '0' else '1')

binToInt :: String -> Int
binToInt = foldl' (\acc x -> if x=='1' then 2*acc + 1 else 2*acc) 0

part1 :: IO Int
part1 = do
  input <- readFile "data/day03.txt"
  let g = gamma (lines input)
  let e = gammaToEpsilon g
  return (binToInt g * binToInt e)
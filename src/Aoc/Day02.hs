{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day02 (
  Instruction(..),
  Command(..),
  Position(..),
  parseCommands,
  run,
  run2,
  part1,
  part2
) where

import Data.Bifunctor (first, bimap)
import Data.List (foldl')
import Data.Text (Text)
import Data.Functor (($>))
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Instruction = Forward | Down | Up deriving (Show, Eq)

data Command = Command { instruction :: Instruction, argument :: Int }
  deriving (Eq)

instance Show Command where
  show (Command instr arg) = show instr ++ "(" ++ show arg ++ ")"

pInstruction :: Parser Instruction
pInstruction =
  string "forward" $> Forward
  <|> string "down" $> Down
  <|> string "up" $> Up

pCommand :: Parser Command
pCommand =
  Command <$> (pInstruction <* space) <*> L.decimal

pCommands :: Parser [Command]
pCommands = (pCommand `sepBy` newline) <* eof

parseCommands :: Text -> Either String [Command]
parseCommands = first show . parse pCommands ""

data Position = Position { horizontal :: Int, depth :: Int, aim :: Int }
  deriving (Eq)

instance Show Position where
  show (Position h d a) = "Position (h: " ++ show h ++ ", d: " ++ show d ++ ", aim: " ++ show a ++ ", mult: " ++ show (h*d) ++ ")"

run :: [Command] -> Position
run =
  let step (Position h d _) (Command instr arg) =
        case instr of
          Forward -> Position (h + arg) d 0
          Down -> Position h (d + arg) 0
          Up -> Position h (d - arg) 0
  in foldl' step (Position 0 0 0)

runAndDescribe :: ([Command] -> Position) -> Text -> String
runAndDescribe runner =
  either show (show . runner) . parseCommands

part1 :: IO String
part1 = runAndDescribe run <$> T.readFile "data/day02.txt"

run2 :: [Command] -> Position
run2 =
  let step (Position h d a) (Command instr arg) =
        case instr of
          Forward -> Position (h + arg) (d + arg * a) a
          Down -> Position h d (a + arg)
          Up -> Position h d (a - arg)
  in foldl' step (Position 0 0 0)

part2 :: IO String
part2 = runAndDescribe run2 <$> T.readFile "data/day02.txt"
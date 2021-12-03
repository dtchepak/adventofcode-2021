{-# LANGUAGE OverloadedStrings #-}
module Aoc.Day02 (
  Instruction(..),
  Command(..),
  Position(..),
  parseCommands,
  run,
  part1
) where

import Data.Bifunctor (first, bimap)
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

data Position = Position { horizontal :: Int, depth :: Int }
  deriving (Eq)

instance Show Position where
  show (Position h d) = "Position (h: " ++ show h ++ ", d: " ++ show d ++ ", mult: " ++ show (h*d) ++ ")"

run :: [Command] -> Position
run =
  let step (Command instr arg) (Position h d) =
        case instr of
          Forward -> Position (h + arg) d
          Down -> Position h (d + arg)
          Up -> Position h (d - arg)
  in foldr step (Position 0 0)

runAndDescribe :: Text -> String
runAndDescribe =
  either show (show . run) . parseCommands

part1 :: IO String
part1 = runAndDescribe <$> T.readFile "data/day02.txt"
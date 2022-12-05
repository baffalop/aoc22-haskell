module Day05 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser)
import Parsing (linesOf)
import Data.Maybe (catMaybes)
import Data.List (transpose)
import Control.Applicative ((<|>))

data Input = Input
  { _stacks :: Stacks
  , _instructions :: [Instruction]
  } deriving (Show)

type Stacks = [String]

data Instruction = Instruction
  { _take :: Int
  , _from :: Int
  , _to :: Int
  } deriving (Show)

parse :: Text -> Either String Input
parse = P.parseOnly $ Input
  <$> stacks
  <* P.skipSpace <* indices <* P.skipSpace
  <*> linesOf instruction
  where
    stacks :: Parser Stacks
    stacks = fmap catMaybes . transpose <$> linesOf (stack `P.sepBy` P.char ' ')

    stack :: Parser (Maybe Char)
    stack =
      Just <$ P.char '[' <*> P.letter <* P.char ']'
        <|> Nothing <$ P.string "   "

    indices :: Parser [Int]
    indices = P.decimal `P.sepBy` P.skipSpace

    instruction :: Parser Instruction
    instruction = Instruction
      <$ P.string "move " <*> P.decimal
      <* P.string " from " <*> P.decimal
      <* P.string " to " <*> P.decimal


solve1 = undefined
solve2 = undefined

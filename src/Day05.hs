module Day05 (parse, solve1, solve2) where

import Data.Text (Text)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser)
import Parsing (linesOf)
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (transpose, uncons)
import Control.Applicative ((<|>))
import Data.Foldable (foldl')

data Input = Input
  { stacks :: Stacks
  , instructions :: [Instruction]
  } deriving (Show)

type Stacks = Seq String

data Instruction = Instruction
  { n :: Int
  , from :: Int
  , to :: Int
  } deriving (Show)

parse :: Text -> Either String Input
parse = P.parseOnly $ Input
  <$> stacks
  <* P.skipSpace <* indices <* P.skipSpace
  <*> linesOf instruction
  where
    stacks :: Parser Stacks
    stacks = Seq.fromList . fmap catMaybes . transpose <$> linesOf (stack `P.sepBy` P.char ' ')

    stack :: Parser (Maybe Char)
    stack = Just <$ P.char '[' <*> P.letter <* P.char ']'
      <|> Nothing <$ P.string "   "

    indices :: Parser [Int]
    indices = P.decimal `P.sepBy` P.skipSpace

    instruction :: Parser Instruction
    instruction = Instruction
      <$ P.string "move " <*> P.decimal
      <* P.string " from " <*> P.decimal
      <* P.string " to " <*> P.decimal


solve1 :: Input -> String
solve1 Input{ stacks, instructions } =
  catMaybes $ toList $ headMay <$> foldl' (flip run) stacks instructions

run :: Instruction -> Stacks -> Stacks
run Instruction{ n, from, to } = nTimes n apply
  where
    apply stacks = fromMaybe stacks $ do
      (top, rest) <- uncons =<< Seq.lookup (from - 1) stacks
      pure $ Seq.adjust' (top :) (to - 1) $ Seq.update (from - 1) rest stacks

solve2 = undefined

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f x
  | n <= 0 = x
  | otherwise = nTimes (n - 1) f $ f x

toList :: Seq a -> [a]
toList = foldr (:) []

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x : _) = Just x

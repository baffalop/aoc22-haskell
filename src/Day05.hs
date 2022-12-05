module Day05 (parse, solve1, solve2) where

import Data.Text (Text)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser)
import Parsing (linesOf)
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (transpose)
import Control.Applicative ((<|>))
import Data.Foldable (foldl', toList)

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
    stacks = Seq.fromList . fmap catMaybes . transpose <$> linesOf (crate `P.sepBy` P.char ' ')

    crate :: Parser (Maybe Char)
    crate =
      Just <$ P.char '[' <*> P.letter <* P.char ']'
      <|> Nothing <$ P.string "   "

    indices :: Parser [Int]
    indices = P.decimal `P.sepBy` P.skipSpace

    instruction :: Parser Instruction
    instruction = Instruction
      <$ P.string "move " <*> P.decimal
      <* P.string " from " <*> (subtract 1 <$> P.decimal)
      <* P.string " to " <*> (subtract 1 <$> P.decimal)

solve1 :: Input -> String
solve1 = solveWith reverse

solve2 :: Input -> String
solve2 = solveWith id

solveWith :: (String -> String) -> Input -> String
solveWith tweak Input{ stacks, instructions } =
  catMaybes $ toList $ headMay <$> foldl' (flip $ runWith tweak) stacks instructions

runWith :: (String -> String) -> Instruction -> Stacks -> Stacks
runWith tweak Instruction{ n, from, to } stacks =
  fromMaybe stacks $ do
    (top, rest) <- splitAt n <$> Seq.lookup from stacks
    pure $ Seq.adjust' (tweak top <>) to $ Seq.update from rest stacks

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x : _) = Just x

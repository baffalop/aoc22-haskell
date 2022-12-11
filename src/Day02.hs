module Day02 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Parsing (linesOf, pairBy)
import Data.Tuple.Extra (second)
import Control.Applicative ((<|>))

data Move = Rock | Paper | Scissors
  deriving (Eq, Bounded, Enum, Show)

beats :: Move -> Move
beats = succWrap

beatenBy :: Move -> Move
beatenBy = predWrap

succWrap :: (Eq a, Bounded a, Enum a) => a -> a
succWrap x
  | x == maxBound = minBound
  | otherwise = succ x

predWrap :: (Eq a, Bounded a, Enum a) => a -> a
predWrap x
  | x == minBound = maxBound
  | otherwise = pred x

data Strategy = Lose | Draw | Win

parse :: Text -> Either String [(Move, Move)]
parse = P.parseOnly $ linesOf $ pairBy ' ' $ P.choice
  [ Rock <$ (P.char 'A' <|> P.char 'X')
  , Paper <$ (P.char 'B' <|> P.char 'Y')
  , Scissors <$ (P.char 'C' <|> P.char 'Z')
  ]

solve1 :: [(Move, Move)] -> Int
solve1 = sum . fmap score

solve2 :: [(Move, Move)] -> Int
solve2 = sum . fmap (score . applyStrategy . second reinterpretAsStrategy)

reinterpretAsStrategy :: Move -> Strategy
reinterpretAsStrategy = \case
  Rock -> Lose
  Paper -> Draw
  Scissors -> Win

applyStrategy :: (Move, Strategy) -> (Move, Move)
applyStrategy (move, strategy) = (move, response move)
  where
    response = case strategy of
      Lose -> beatenBy
      Draw -> id
      Win -> beats

score :: (Move, Move) -> Int
score (x, y) = scoreMove y + scoreWin x y
  where
    scoreMove move = fromEnum move + 1

    scoreWin a b
      | b == a = 3
      | b == beats a = 6
      | otherwise = 0

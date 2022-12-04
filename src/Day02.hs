module Day02 (parse, solve1, solve2) where

import Data.Text (Text, unpack)
import Data.Tuple.Extra (first, second)

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

parse :: Text -> [(Move, String)]
parse = fmap (first parseMove . pair . words) . lines . unpack
  where
    pair :: (Show a) => [a] -> (a, a)
    pair [x, y] = (x, y)
    pair input = error $ "Not two words encountered: " <> show input

solve1 :: [(Move, String)] -> Int
solve1 = sum . fmap (score . second parseMove)

solve2 :: [(Move, String)] -> Int
solve2 = sum . fmap (score . applyStrategy . second parseStrategy)

parseMove :: String -> Move
parseMove "A" = Rock
parseMove "B" = Paper
parseMove "C" = Scissors
parseMove "X" = Rock
parseMove "Y" = Paper
parseMove "Z" = Scissors
parseMove input = error $ "Cannot parse " <> input <> " as Move"

parseStrategy :: String -> Strategy
parseStrategy "X" = Lose
parseStrategy "Y" = Draw
parseStrategy "Z" = Win
parseStrategy input = error $ "Cannot parse " <> input <> " as Strategy"

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

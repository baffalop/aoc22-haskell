module Day02 (parse, solve1, solve2) where

import Data.Text (Text, unpack)
import Data.Tuple.Extra (firstM, secondM, second)
import Control.Monad ((>=>), (<=<))

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
parse = traverse (bothM parseMove <=< pair . words) . lines . unpack
  where
    pair :: (Show a) => [a] -> Either String (a, a)
    pair [x, y] = Right (x, y)
    pair input = Left $ "Not two words encountered: " <> show input

solve1 :: [(Move, Move)] -> Int
solve1 = sum . fmap score

solve2 :: [(Move, Move)] -> Int
solve2 = sum . fmap (score . applyStrategy . second moveToStrategy)

parseMove :: String -> Either String Move
parseMove "A" = Right Rock
parseMove "B" = Right Paper
parseMove "C" = Right Scissors
parseMove "X" = Right Rock
parseMove "Y" = Right Paper
parseMove "Z" = Right Scissors
parseMove input = Left $ "Cannot parse " <> input <> " as Move"

moveToStrategy :: Move -> Strategy
moveToStrategy Rock = Lose
moveToStrategy Paper = Draw
moveToStrategy Scissors = Win

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

bothM :: Monad m => (a -> m b) -> (a, a) -> m (b, b)
bothM f = firstM f >=> secondM f

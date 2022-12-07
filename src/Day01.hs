module Day01 (parse, solve1, solve2) where

import Data.Text (Text, unpack)
import Data.List (sortOn, break)
import Utils ((<.>))

parse :: Text -> [Int]
parse = sum . fmap read <.> splitOn "" . lines . unpack

solve1 :: [Int] -> Int
solve1 = maximum

solve2 :: [Int] -> Int
solve2 = sum . take 3 . sortOn negate

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn x xs =
  let (grouped, rest) = break (== x) xs in
  grouped : case rest of
    [] -> []
    _ : remainder -> splitOn x remainder

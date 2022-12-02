module Day01 (parse, solve1, solve2) where

import Data.List (sortOn, break)

parse :: String -> [Int]
parse = fmap (sum . fmap read) . splitOn "" . lines

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

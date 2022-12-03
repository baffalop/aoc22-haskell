module Day03 (parse, solve1, solve2) where

import Data.List (intersect)
import Data.List.Extra (chunksOf)
import Data.Char (ord)

parse :: String -> [String]
parse = lines

solve1 :: [String] -> Int
solve1 = priorities . fmap (head . uncurry intersect . halves)

solve2 :: [String] -> Int
solve2 = priorities . fmap (head . foldr1 intersect) . chunksOf 3

priorities :: String -> Int
priorities = sum . fmap priority

priority :: Char -> Int
priority c =
  let code = ord c in
  if code > 96 then code - 96 else code - 38

halves :: [a] -> ([a], [a])
halves xs = splitAt (length xs `div` 2) xs

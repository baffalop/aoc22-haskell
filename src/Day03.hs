module Day03 (parse, solve1, solve2) where

import Data.Set (Set, intersection, union)
import qualified Data.Set as Set
import Data.List (splitAt)
import Data.List.Extra (chunksOf)
import Data.Tuple.Extra (both)
import Data.Char (ord)

type Bag = (Set Char, Set Char)

parse :: String -> [Bag]
parse = fmap (both Set.fromList . halves) . lines

solve1 :: [Bag] -> Int
solve1 = priorities . fmap (uncurry intersection)

solve2 :: [Bag] -> Int
solve2 = priorities . fmap (foldr1 intersection) . chunksOf 3 . fmap (uncurry union)

priorities :: [Set Char] -> Int
priorities = sum . fmap priority . concatMap Set.toList

priority :: Char -> Int
priority c =
  let code = ord c in
  if code > 96 then code - 96 else code - 38

halves :: [a] -> ([a], [a])
halves xs = splitAt (length xs `div` 2) xs

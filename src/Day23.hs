module Day23 (parse, solve1, solve2) where

import Data.Text (Text, unpack)
import Data.Set (Set)
import qualified Data.Set as Set

type Input = Set Coord
type Coord = (Int, Int)

parse :: Text -> Input
parse = ifoldr build Set.empty . lines . unpack
  where
    build :: (Int, String) -> Set Coord -> Set Coord
    build (y, line) coords = ifoldr \case
        (x, '#') -> Set.insert (x, y)
        _ -> id
      coords line

solve1 :: Input -> Int
solve1 = undefined

solve2 :: Input -> Int
solve2 = undefined

ifoldr :: ((Int, a) -> b -> b) -> b -> [a] -> b
ifoldr f initial = foldr f initial . zip [0..]

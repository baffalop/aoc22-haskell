module Day14 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Parsing (pairBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Utils (partitionWith, pairs)
import Data.Tuple.Extra (both)

type Path = [Coord]
type Line = (Coord, Coord)
type Coord = (Int, Int)

data Cave = Cave
  { rows :: Cardinal
  , cols :: Cardinal
  } deriving (Show)

type Cardinal = Map Int (Set Int)

parse :: Text -> Either String Cave
parse = P.parseOnly $ mapCave <$>
  (pairBy ',' P.decimal `P.sepBy` P.string " -> ") `P.sepBy` P.endOfLine

solve1 :: Cave -> Int
solve1 = undefined

solve2 :: Cave -> Int
solve2 = undefined

mapCave :: [Path] -> Cave
mapCave =
  uncurry Cave . both cardinal . partitionWith orientation . foldMap pairs
  where
    orientation :: Line -> Either (Int, Set Int) (Int, Set Int)
    orientation ((x1, y1), (x2, y2))
      | x1 == x2 = Left (x1, Set.fromList [y1..y2])
      | otherwise = Right (y1, Set.fromList [x1..x2])

    cardinal :: [(Int, Set Int)] -> Cardinal
    cardinal = foldr (uncurry $ Map.insertWith Set.union) Map.empty

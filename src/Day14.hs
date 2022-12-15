module Day14 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Parsing (pairBy)
import Data.IntMap (IntMap, (!?))
import qualified Data.IntMap as Map
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Utils (pairs)
import Data.Maybe (fromMaybe)
import Data.List (find)
import Data.Either.Extra (maybeToEither)

type Path = [Coord]
type Line = (Coord, Coord)
type Coord = (Int, Int)

type Cave = IntMap IntSet

parse :: Text -> Either String Cave
parse = P.parseOnly $ mapCave <$>
  (pairBy ',' P.decimal `P.sepBy` P.string " -> ") `P.sepBy` P.endOfLine

solve1 :: Cave -> Int
solve1 = pour 0
  where
    pour :: Int -> Cave -> Int
    pour count cave = case flowsIn cave (500, 0) of
      Left _ -> count
      Right next -> pour (count + 1) $ next `addTo` cave

solve2 :: Cave -> Int
solve2 cave = pour 0 cave
  where
    pour :: Int -> Cave -> Int
    pour count cave
      | (500, 0) `dropsToIn` cave == Just (500, 0) = count + 1
      | otherwise = pour (count + 1) $ (`addTo` cave) $
        case flowsIn cave (500, 0) of
          Left x -> (x, floorLevel)
          Right next -> next

    floorLevel :: Int
    floorLevel = Set.findMax (foldr1 (<>) cave) + 1

mapCave :: [Path] -> Cave
mapCave = Map.fromListWith (<>) . foldMap columns . foldMap pairs
  where
    columns :: Line -> [(Int, IntSet)]
    columns ((x1, y1), (x2, y2))
      | x1 == x2 = [(x1, Set.fromList [min y1 y2..max y1 y2])]
      | otherwise = (, Set.singleton y1) <$> [min x1 x2..max x1 x2]

flowsIn :: Cave -> Coord -> Either Int Coord
flowsIn cave = flow
  where
    flow :: Coord -> Either Int Coord
    flow c = do
      bottom@(x, y) <- maybeToEither (fst c) $ c `dropsToIn` cave
      case filter (not . (`blockedBy` cave)) $ (, y + 1) <$> [x - 1, x + 1] of
        next:_ -> flow next
        _ -> return bottom

dropsToIn :: (Int, Int) -> Cave -> Maybe Coord
dropsToIn (x, y) cave = do
  col <- cave !? x
  ground <- find (> y) $ Set.toList col
  return (x, ground - 1)

blockedBy :: Coord -> Cave -> Bool
blockedBy (x, y) cave = possibly $ Set.member y <$> cave !? x

addTo :: Coord -> Cave -> Cave
addTo (x, y) = Map.insertWith (<>) x $ Set.singleton y

possibly :: Maybe Bool -> Bool
possibly = fromMaybe False

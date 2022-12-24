module Day14 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Parsing (pairBy, linesOf)
import Data.IntMap (IntMap, (!?))
import qualified Data.IntMap as Map
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Utils (pairs)
import Data.Maybe (fromMaybe)
import Data.Either.Extra (maybeToEither)
import Data.Foldable (find)
import Data.Functor ((<&>))

type Path = [Coord]
type Line = (Coord, Coord)
type Coord = (Int, Int)

type Cave = IntMap IntSet

parse :: Text -> Either String Cave
parse = P.parseOnly $
  mapCave <$> linesOf (pairBy ',' P.decimal `P.sepBy` P.string " -> ")

solve1 :: Cave -> Int
solve1 = pour 0
  where
    pour :: Int -> Cave -> Int
    pour count cave = case flowsIn cave (500, 0) of
      Left _ -> count
      Right next -> pour (count + 1) $ insert next cave

solve2 :: Cave -> Int
solve2 initCave = pour 1 initCave
  where
    pour :: Int -> Cave -> Int
    pour count cave = case flowsIn cave (500, 0) of
      Left x -> pour (count + 1) $ insert (x, floorLevel) cave
      Right (500, 0) -> count
      Right next -> pour (count + 1) $ insert next cave

    floorLevel :: Int
    floorLevel = Set.findMax (foldr1 (<>) initCave) + 1

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
    flow c@(originX, _) = do
      bottom@(x, y) <- maybeToEither originX $ c `dropsToIn` cave
      case find (not . (`blockedBy` cave)) $ [x - 1, x + 1] <&> (, y + 1) of
        Just next -> flow next
        Nothing -> return bottom

dropsToIn :: (Int, Int) -> Cave -> Maybe Coord
dropsToIn (x, y) cave = do
  col <- cave !? x
  ground <- Set.lookupGT y col
  return (x, ground - 1)

blockedBy :: Coord -> Cave -> Bool
blockedBy (x, y) cave = possibly $ Set.member y <$> cave !? x

insert :: Coord -> Cave -> Cave
insert (x, y) = Map.insertWith (<>) x $ Set.singleton y

possibly :: Maybe Bool -> Bool
possibly = fromMaybe False

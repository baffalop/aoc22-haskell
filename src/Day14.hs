{-# LANGUAGE TemplateHaskell #-}

module Day14 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Parsing (pairBy)
import Data.IntMap (IntMap, (!?))
import qualified Data.IntMap as Map
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
import Utils (partitionWith, pairs, (<.>))
import Data.Tuple.Extra (both)
import Data.Maybe (fromMaybe)
import Lens.Micro.Platform (makeLensesFor, (%~))
import Data.List (find)

type Path = [Coord]
type Line = (Coord, Coord)
type Coord = (Int, Int)

data Cave = Cave
  { rows :: Cardinal
  , cols :: Cardinal
  } deriving (Show)

type Cardinal = IntMap IntSet

makeLensesFor [("rows", "_rows"), ("cols", "_cols")] ''Cave

parse :: Text -> Either String Cave
parse = P.parseOnly $ mapCave <$>
  (pairBy ',' P.decimal `P.sepBy` P.string " -> ") `P.sepBy` P.endOfLine

solve1 :: Cave -> Int
solve1 = pour 0
  where
    pour :: Int -> Cave -> Int
    pour count cave = case flowsIn cave (500, 0) of
      Nothing -> count
      Just next -> pour (count + 1) $ insertAt next cave

solve2 :: Cave -> Int
solve2 = undefined

mapCave :: [Path] -> Cave
mapCave =
  uncurry Cave . both cardinal . partitionWith orientation . foldMap pairs
  where
    orientation :: Line -> Either (Int, IntSet) (Int, IntSet)
    orientation ((x1, y1), (x2, y2))
      | y1 == y2 = Left (y1, Set.fromList [min x1 x2..max x1 x2])
      | otherwise = Right (x1, Set.fromList [min y1 y2..max y1 y2])

    cardinal :: [(Int, IntSet)] -> Cardinal
    cardinal = foldr (uncurry $ Map.insertWith Set.union) Map.empty

flowsIn :: Cave -> Coord -> Maybe Coord
flowsIn cave = flow
  where
    flow :: Coord -> Maybe Coord
    flow c = do
      bottom@(x, y) <- dropsTo c cave
      case filter (not . blockedBy cave) $ (, y + 1) <$> [x - 1, x + 1] of
        next:_ -> flow next
        _ -> return bottom

dropsTo :: (Int, Int) -> Cave -> Maybe Coord
dropsTo (x, y) Cave{..} =
  (x,) . subtract 1 <.> find (> y) $ Set.toList $ verticals <> horizontals
  where
    verticals = fromMaybe Set.empty (cols !? x)
    horizontals = Map.keysSet $ Map.filter (Set.member x) rows

blockedBy :: Cave -> Coord -> Bool
blockedBy Cave{ rows, cols } (x, y) =
  possibly (Set.member x <$> rows !? y) || possibly (Set.member y <$> cols !? x)

insertAt :: Coord -> Cave -> Cave
insertAt (x, y) =
    (_rows %~ Map.insertWith (<>) y (Set.singleton x))
  . (_cols %~ Map.insertWith (<>) x (Set.singleton y))

possibly :: Maybe Bool -> Bool
possibly = fromMaybe False

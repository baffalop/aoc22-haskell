module Day23 (parse, solve1, solve2) where

import Data.Text (Text, unpack)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Foldable (find)
import Control.Arrow ((&&&))

type Input = Set Coord
type Coord = (Int, Int)

data Dir = North | South | West | East
  deriving (Show, Enum)

parse :: Text -> Input
parse = ifoldr build Set.empty . lines . unpack
  where
    build :: (Int, String) -> Set Coord -> Set Coord
    build (y, line) coords = ifoldr \case
        (x, '#') -> Set.insert (x, y)
        _ -> id
      coords line

solve1 :: Input -> Int
solve1 = emptySpace . run 10 [North .. East]

solve2 :: Input -> Int
solve2 = undefined

run :: Int -> [Dir] -> Set Coord -> Set Coord
run 0 _ coords = coords
run n dirs coords =
  run (n - 1) (rotate dirs)
    $ Map.foldrWithKey makeMove coords
    $ foldr addMove Map.empty coords
  where
    makeMove :: Coord -> [Coord] -> Set Coord -> Set Coord
    makeMove new [old] = Set.delete old . Set.insert new
    makeMove _ _ = id

    addMove :: Coord -> Map Coord [Coord] -> Map Coord [Coord]
    addMove coord =
      maybe id (append coord . add coord . toVector) $ find (all isFree . lookIn coord) dirs

    isFree :: Coord -> Bool
    isFree = not . (`Set.member` coords)

emptySpace :: Set Coord -> Int
emptySpace coords = area coords - Set.size coords

area :: Set Coord -> Int
area coords =
  let (xs, ys) = (Set.map fst &&& Set.map snd) coords in
  (Set.findMax xs - Set.findMin xs + 1) * (Set.findMax ys - Set.findMin ys + 1)

lookIn :: Coord -> Dir -> [Coord]
lookIn coord dir = add coord . inDirection dir <$> [-1 .. 1]

toVector :: Dir -> Coord
toVector dir = inDirection dir 0

inDirection :: Dir -> Int -> Coord
inDirection = \case
  North -> (,-1)
  South -> (,1)
  West -> (-1,)
  East -> (1,)

rotate :: [Dir] -> [Dir]
rotate = take 4 . drop 1 . cycle

append :: Ord k => a -> k -> Map k [a] -> Map k [a]
append x k = flip Map.alter k $ Just . (x :) . fromMaybe []

ifoldr :: ((Int, a) -> b -> b) -> b -> [a] -> b
ifoldr f initial = foldr f initial . zip [0..]

add :: Coord -> Coord -> Coord
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

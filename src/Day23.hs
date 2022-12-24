module Day23 (parse, solve1, solve2, viz) where

import Data.Text (Text, unpack)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Foldable (find)
import Control.Arrow ((&&&))
import Data.Matrix (Matrix)
import qualified Data.Matrix as Mx
import Data.Tuple.Extra (both)
import Control.Monad (guard)
import Utils (nTimes, (<.>), pairs)
import Data.List (findIndex)
import Data.List.Extra (enumerate)

type Input = Set Coord
type Coord = (Int, Int)

data Dir = North | South | West | East
  deriving (Show, Enum, Bounded)

parse :: Text -> Input
parse = ifoldr build Set.empty . lines . unpack
  where
    build :: (Int, String) -> Set Coord -> Set Coord
    build (y, line) coords = ifoldr \case
        (x, '#') -> Set.insert (x, y)
        _ -> id
      coords line

solve1 :: Input -> Int
solve1 = emptySpace . nTimesWith 10 enumerate rotate disperse

solve2 :: Input -> Maybe Int
solve2 = (+ 1) <.> findIndex (uncurry (==)) . pairs . iterateWith enumerate rotate disperse

disperse :: [Dir] -> Set Coord -> Set Coord
disperse dirs coords =
  Map.foldrWithKey makeMove coords $ foldr proposeMove Map.empty coords
  where
    makeMove :: Coord -> [Coord] -> Set Coord -> Set Coord
    makeMove new [old] = Set.delete old . Set.insert new
    makeMove _ _ = id

    proposeMove :: Coord -> Map Coord [Coord] -> Map Coord [Coord]
    proposeMove coord = fromMaybe id do
      guard $ not $ all isFree $ foldMap (lookFrom coord) dirs
      dir <- find (all isFree . lookFrom coord) dirs
      return $ append coord $ moveIn dir coord

    isFree :: Coord -> Bool
    isFree = not . (`Set.member` coords)

emptySpace :: Set Coord -> Int
emptySpace coords = area coords - Set.size coords

area :: Set Coord -> Int
area coords =
  let (xs, ys) = (Set.map fst &&& Set.map snd) coords in
  (Set.findMax xs - Set.findMin xs + 1) * (Set.findMax ys - Set.findMin ys + 1)

lookFrom :: Coord -> Dir -> [Coord]
lookFrom coord dir = add coord . inDirection dir <$> [-1 .. 1]

moveIn :: Dir -> Coord -> Coord
moveIn dir = add $ inDirection dir 0

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

add :: Coord -> Coord -> Coord
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

ifoldr :: ((Int, a) -> b -> b) -> b -> [a] -> b
ifoldr f initial = foldr f initial . zip [0..]

nTimesWith :: Int -> b -> (b -> b) -> (b -> a -> a) -> a -> a
nTimesWith n initial g f = snd . nTimes n (\(b, a) -> (g b, f b a)) . (initial,)

iterateWith :: b -> (b -> b) -> (b -> a -> a) -> a -> [a]
iterateWith initial g f = snd <.> iterate (\(b, a) -> (g b, f b a)) . (initial,)

-- debugging

data Sq = Elf | Empty
instance Show Sq where
  show Elf = "#"
  show Empty = "."

viz :: Set Coord -> Matrix Sq
viz coords =
  Mx.matrix height width \(y, x) ->
    if Set.member (x + minX - 1, y + minY - 1) coords then Elf else Empty
  where
    dimensions@(xs, ys) = (Set.map fst &&& Set.map snd) coords
    (minX, minY) = both Set.findMin dimensions
    (width, height) = (Set.findMax xs - minX + 1, Set.findMax ys - minY + 1)

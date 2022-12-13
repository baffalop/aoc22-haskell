module Day12 (parse, solve1, solve2) where

import Prelude hiding (map)
import Data.Text (Text, unpack)
import Data.Char (ord)
import Data.Matrix (Matrix, (!))
import qualified Data.Matrix as Mx
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as Q
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Either.Extra (maybeToEither)
import Data.Function ((&))
import Data.List (elemIndex, nub)
import Data.Maybe (isJust)
import Utils (indexedFind, within)
import Data.Tuple.Extra (both)
import Control.Monad ((>=>))

type Terrain = Matrix Int
type Coord = (Int, Int)

data Hill = Hill
  { terrain :: Terrain
  , start :: Coord
  , end :: Coord
  } deriving (Show)

parse :: Text -> Either String Hill
parse (lines . unpack -> map) = do
  start <- findCoord 'S' & maybeToEither "Could not find start"
  end <- findCoord 'E' & maybeToEither "Could not find end"
  let terrain = fmap (subtract 97 . ord) $ Mx.setElem 'a' start $ Mx.setElem 'z' end $ Mx.fromLists map
  return Hill{..}
  where
    findCoord :: Char -> Maybe Coord
    findCoord c = do
      (y, maybeX) <- indexedFind isJust $ elemIndex c <$> map
      x <- maybeX
      return $ both (+ 1) (y, x)

solve1 :: Hill -> Maybe Int
solve1 Hill{..} = shortestPathDijkstra end (== start) terrain

solve2 :: Hill -> Maybe Int
solve2 Hill{..} = shortestPathDijkstra end ((== 0) . (terrain !)) terrain

shortestPathDijkstra :: Coord -> (Coord -> Bool) -> Terrain -> Maybe Int
shortestPathDijkstra from isTarget terrain = search Set.empty $ Q.singleton 0 from
  where
    search :: Set Coord -> MinPQueue Int Coord -> Maybe Int
    search visited = Q.minViewWithKey >=> \((score, cur), unvisited) ->
      if isTarget cur then return score
      else if cur `Set.member` visited then search visited unvisited
      else search (Set.insert cur visited)
        $ foldr (Q.insert $ score + 1) unvisited
        $ filter (not . (`Set.member` visited)) $ neighbours cur terrain

neighbours :: Coord -> Terrain -> [Coord]
neighbours coord@(row, col) terrain = nub do
  row' <- filter (`within` (1, Mx.nrows terrain)) [row - 1, row + 1]
  col' <- filter (`within` (1, Mx.ncols terrain)) [col - 1, col + 1]
  flip filter [(row', col), (row, col')] \c -> h - terrain ! c <= 1
  where h = terrain ! coord

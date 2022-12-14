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
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import Data.Either.Extra (maybeToEither)
import Data.Function ((&))
import Data.List (elemIndex, nub)
import Data.Maybe (isJust)
import Utils (indexedFind, within)
import Data.Tuple.Extra (both, uncurry3)
import Control.Monad ((>=>))
import Data.Function.Flip (flip3)
import Data.Functor ((<&>))
import Lens.Micro.Platform (_1, _2, _3, ix, (%~))
import qualified Debug.Trace

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
  let terrain = fmap (subtract (ord 'a') . ord) $ Mx.setElem 'a' start $ Mx.setElem 'z' end $ Mx.fromLists map
  return Hill{..}
  where
    findCoord :: Char -> Maybe Coord
    findCoord c = do
      (y, maybeX) <- indexedFind isJust $ elemIndex c <$> map
      x <- maybeX
      return $ both (+ 1) (y, x)

solve1 :: Hill -> Maybe Int
solve1 Hill{..} = shortestPathDijkstra start (== end) $ neighboursIn terrain subtract

solve2 :: Hill -> Maybe Int
solve2 Hill{..} = shortestPathDijkstra end ((== 0) . (terrain !)) $ neighboursIn terrain (-)

shortestPathDijkstra :: Coord -> (Coord -> Bool) -> (Coord -> [Coord]) -> Maybe Int
shortestPathDijkstra from isTarget neighbours = search Set.empty $ Q.singleton 0 from
  where
    search :: Set Coord -> MinPQueue Int Coord -> Maybe Int
    search visited = Q.minViewWithKey >=> \((score, cur), candidates) ->
      -- Debug.Trace.traceShowM $ debugMatrix terrain $ Set.toList visited
      if isTarget cur then return score
      else if cur `Set.member` visited then search visited candidates
      else search (Set.insert cur visited)
        $ foldr (Q.insert $ score + 1) candidates
        $ filter (not . (`Set.member` visited)) $ neighbours cur

shortestPathAStar :: Coord -> Coord -> (Coord -> [Coord]) -> Maybe Int
shortestPathAStar start end neighbours =
  search (Map.singleton start 0) (Map.singleton start $ distance start end) (Q.singleton 0 start)
  where
    search :: Map Coord Int -> Map Coord Int -> MinPQueue Int Coord -> Maybe Int
    search pathScores heuristics (Q.minView -> candidatesView) = do
      (cur, candidates) <- candidatesView
      -- Debug.Trace.traceShowM $ debugMatrix terrain $ Map.keys pathScores
      if cur == end then pathScores !? cur
      else do
        nextPathScore <- pathScores !? cur <&> (+ 1)
        let nextCandidates = flip filter (neighbours cur) \neighbour ->
              pathScores !? neighbour & maybe True (> nextPathScore)
        uncurry3 search $ flip3 foldr nextCandidates (pathScores, heuristics, candidates)
          \candidate ->
            let nextHeuristic = nextPathScore + distance candidate end in
              (_1 %~ Map.insert candidate nextPathScore)
            . (_2 %~ Map.insert candidate nextHeuristic)
            . (_3 %~ Q.insert nextHeuristic candidate)

neighboursIn :: Terrain -> (Int -> Int -> Int) -> Coord -> [Coord]
neighboursIn terrain gradient coord@(row, col) = nub do
  row' <- filter (`within` (1, Mx.nrows terrain)) [row - 1, row + 1]
  col' <- filter (`within` (1, Mx.ncols terrain)) [col - 1, col + 1]
  flip filter [(row', col), (row, col')] \c -> gradient h (terrain ! c) <= 1
  where h = terrain ! coord

distance :: Coord -> Coord -> Int
distance (y1, x1) (y2, x2) = abs (y2 - y1) + abs (x2 - x1)

{- Debug tracing:

data DebugLoc = Height Int | Visited Int
instance Show DebugLoc where
  show = \case
    (Height v) -> show v
    (Visited (show -> [v])) -> [v, '#']
    (Visited (show -> (_:v))) -> '#':v
    _ -> ""

visit :: DebugLoc -> DebugLoc
visit (Height v) = Visited v
visit x = x

debugMatrix :: Terrain -> [Coord] -> Matrix DebugLoc
debugMatrix terrain visited = Mx.fromLists $
  flip3 foldr visited (Mx.toLists $ Height <$> terrain)
    \(both (subtract 1) -> (row, col)) -> ix row . ix col %~ visit

-}

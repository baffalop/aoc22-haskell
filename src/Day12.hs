{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}

module Day12 (parse, solve1, solve2) where

import Prelude hiding (map)
import Data.Text (Text, unpack)
import Data.Char (ord)
import Data.Matrix (Matrix, (!))
import qualified Data.Matrix as Mx
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as Q
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import Data.Either.Extra (maybeToEither)
import Data.Function ((&))
import Data.List (elemIndex, nub)
import Data.Maybe (isJust)
import Data.Tuple.Extra (both)
import Data.Function.Flip ((<-->))
import Utils (indexedFind, within)
import Data.Functor ((<&>))
import Lens.Micro.Platform (makeLensesFor, (%~))

type Terrain = Matrix Int
type Coord = (Int, Int)

data Hill = Hill
  { terrain :: Terrain
  , start :: Coord
  , end :: Coord
  } deriving (Show)

data SearchState = Search
  { candidates :: MinPQueue Int Coord
  , pathScores :: Map Coord Int
  , heuristics :: Map Coord Int
  }

makeLensesFor
  [("candidates", "_candidates"), ("pathScores", "_pathScores"), ("heuristics", "_heuristics")]
  ''SearchState

parse :: Text -> Either String Hill
parse (lines . unpack -> map) = do
  start <- findCoord 'S' & maybeToEither "Could not find start"
  end <- findCoord 'E' & maybeToEither "Could not find end"
  let terrain = fmap ord $ Mx.setElem 'a' start $ Mx.setElem 'z' end $ Mx.fromLists map
  pure $ Hill { .. }
  where
    findCoord :: Char -> Maybe Coord
    findCoord c = do
      (y, maybeX) <- indexedFind isJust $ elemIndex c <$> map
      x <- maybeX
      pure $ both (+ 1) (y, x)

solve1 :: Hill -> Maybe Int
solve1 Hill{..} = aStarShortest start end terrain

solve2 :: Hill -> Int
solve2 = undefined

aStarShortest :: Coord -> Coord -> Terrain -> Maybe Int
aStarShortest start end terrain =
  search $ Search
    { candidates = Q.singleton 0 start
    , pathScores = Map.singleton start 0
    , heuristics = Map.singleton start $ distance start end
    }
  where
    search :: SearchState -> Maybe Int
    search Search{ candidates = (Q.minView -> candidatesView), .. } = do
      (cur, candidates) <- candidatesView
      if cur == end then pathScores !? cur
      else do
        nextPathScore <- pathScores !? cur <&> (+ 1)
        let nextCandidates = flip filter (neighbours cur terrain) \neighbour ->
              pathScores !? neighbour & maybe True (> nextPathScore)
        search $ (foldr <--> nextCandidates) Search{..}
          \candidate state ->
            let nextHeuristic = nextPathScore + distance candidate end in
            state
              & _pathScores %~ Map.insert candidate nextPathScore
              & _heuristics %~ Map.insert candidate nextHeuristic
              & _candidates %~ addUnique nextHeuristic candidate

neighbours :: Coord -> Terrain -> [Coord]
neighbours coord@(row, col) terrain = nub do
  row' <- filter (`within` (1, Mx.nrows terrain)) [row - 1, row + 1]
  col' <- filter (`within` (1, Mx.ncols terrain)) [col - 1, col + 1]
  flip filter [(row', col), (row, col')] $ \c -> abs (h - terrain ! c) <= 1
  where h = terrain ! coord

distance :: Coord -> Coord -> Int
distance (y1, x1) (y2, x2) = abs (y2 - y1) + abs (x2 - x1)

addUnique :: (Ord k, Eq a) => k -> a -> MinPQueue k a -> MinPQueue k a
addUnique k a q = if a `elem` Q.elemsU q then q else Q.insert k a q

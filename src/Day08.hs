module Day08 (parse, solve1, solve2) where

import Data.Text (Text, unpack)
import Data.Matrix (Matrix)
import qualified Data.Matrix as Mx
import qualified Data.Vector as V
import Data.List (transpose, findIndex)
import Utils ((<.>))
import Data.Tuple.Extra (both)
import Data.Foldable (toList)
import Data.Function (on)

type Input = [[Int]]
type Output = Int

parse :: Text -> Input
parse = fmap (read . (: [])) <.> lines . unpack

solve1 :: Input -> Output
solve1 grid = sum $ length . filter id <$> zipWith (zipWith (||)) verts (transpose hors)
  where
    (hors, verts) = both (fmap mapVisibleBothSides) (grid, transpose grid)

mapVisibleBothSides :: [Int] -> [Bool]
mapVisibleBothSides = ((zipWith (||) . reverse) `on` mapVisible) <$> reverse <*> id

mapVisible :: [Int] -> [Bool]
mapVisible = zipWith (>) <$> id <*> scanl max (-1)

solve2 :: Input -> Output
solve2 input = maximum . Mx.toList . Mx.mapPos scenicScore $ grid
  where
    scenicScore :: (Int, Int) -> Int -> Int
    scenicScore pos tree = product $ countVisibleFrom tree <$> viewsFrom pos grid

    grid :: Matrix Int
    grid = Mx.fromLists input

countVisibleFrom :: Int -> [Int] -> Int
countVisibleFrom height trees = maybe (length trees) (+ 1) $ findIndex (>= height) trees

viewsFrom :: (Int, Int) -> Matrix a -> [[a]]
viewsFrom (x, y) grid =
  flip foldMap
    [(y, Mx.getRow x grid), (x, Mx.getCol y grid)]
    \(i, vector) -> toList <$>
      [ V.reverse $ V.slice 0 (i - 1) vector
      , V.slice i (size - i) vector
      ]
  where size = Mx.nrows grid

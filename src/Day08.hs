module Day08 (parse, solve1, solve2, countVisibleFrom, views) where

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

solve2 :: Input -> Output
solve2 input = maximum . Mx.toList . Mx.mapPos scenicScore $ grid
  where
    scenicScore :: (Int, Int) -> Int -> Int
    scenicScore pos tree = product $ countVisibleFrom tree <$> views pos grid

    grid :: Matrix Int
    grid = Mx.fromLists input

views :: (Int, Int) -> Matrix a -> [[a]]
views (x, y) grid = toList <$>
  [ V.reverse $ V.slice 0 (y - 1) row
  , V.slice y (size - y) row
  , V.reverse $ V.slice 0 (x - 1) col
  , V.slice x (size - x) col
  ]
  where
    (row, col) = both ($ grid) (Mx.getRow x, Mx.getCol y)
    size = Mx.nrows grid

countVisibleFrom :: Int -> [Int] -> Int
countVisibleFrom viewpoint trees = maybe (length trees) (+ 1) $ findIndex (>= viewpoint) trees

mapVisibleBothSides :: [Int] -> [Bool]
mapVisibleBothSides = ((zipWith (||) . reverse) `on` mapVisible) <$> reverse <*> id

mapVisible :: [Int] -> [Bool]
mapVisible heights = zipWith (>) heights $ scanl max (-1) heights

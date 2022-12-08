module Day08 (parse, solve1, solve2) where

import Data.Text (Text, unpack)
import Data.List (transpose)
import Utils ((<.>))
import Data.Tuple.Extra (both)
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
solve2 = undefined

mapVisibleBothSides :: [Int] -> [Bool]
mapVisibleBothSides = ((zipWith (||) . reverse) `on` mapVisible) <$> reverse <*> id

mapVisible :: [Int] -> [Bool]
mapVisible heights = zipWith (>) heights $ scanl max (-1) heights

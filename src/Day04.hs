module Day04 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Parsing (linesOf, pairBy)
import Utils (within)

type Area = (Int, Int)
type Input = [(Area, Area)]

parse :: Text -> Either String Input
parse = P.parseOnly $ linesOf $ pairBy ',' (pairBy '-' P.decimal)

solve1 :: Input -> Int
solve1 = length . filter (\(x, y) -> x `contains` y || y `contains` x)

solve2 :: Input -> Int
solve2 = length . filter (uncurry overlap)

contains :: Area -> Area -> Bool
contains area (x, y) = all (`within` area) [x, y]

overlap :: Area -> Area -> Bool
overlap a@(x1, y1) b@(x2, y2) = x1 `within` b || y1 `within` b || x2 `within` a || y2 `within` a

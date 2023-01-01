module Day18 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Parsing (linesOf)
import Data.Set (Set)
import qualified Data.Set as Set
import Utils (count)

type Input = Set Coord
type Coord = (Int, Int, Int)

parse :: Text -> Either String Input
parse = P.parseOnly $ fmap Set.fromList $ linesOf $
  (,,) <$> P.decimal <* P.char ',' <*> P.decimal <* P.char ',' <*> P.decimal

solve1 :: Input -> Int
solve1 input = sum $ surface input <$> Set.toList input

solve2 :: Input -> Int
solve2 = undefined

surface :: Set Coord -> Coord -> Int
surface coords = (6 -) . count (`Set.member` coords) . neighbours

neighbours :: Coord -> [Coord]
neighbours c@(x, y, z) = filter ((== 1) . distance c)
  [(x', y', z') | x' <- adj x, y' <- adj y, z' <- adj z]
  where
    adj a = [a - 1 .. a + 1]

distance :: Coord -> Coord -> Int
distance (x1, y1, z1) (x2, y2, z2) = sum $ abs <$> [x2 - x1, y2 - y1, z2 - z1]

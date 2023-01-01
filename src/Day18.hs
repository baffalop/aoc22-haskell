module Day18 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Parsing (linesOf)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Utils (count, within)
import Data.Tuple.Extra (fst3, snd3, thd3, first)
import Control.Arrow ((&&&))

type Input = Set Coord
type Coord = (Int, Int, Int)

parse :: Text -> Either String Input
parse = P.parseOnly $ fmap Set.fromList $ linesOf $
  (,,) <$> P.decimal <* P.char ',' <*> P.decimal <* P.char ',' <*> P.decimal

solve1 :: Input -> Int
solve1 cubes = sum $ surface <$> Set.toList cubes
  where
    surface :: Coord -> Int
    surface = (6 -) . count (`Set.member` cubes) . neighbours

solve2 :: Input -> Int
solve2 cubes = fst $ reachableFrom (fst xBounds, fst yBounds, fst zBounds) Set.empty
  where
    reachableFrom :: Coord -> Set Coord -> (Int, Set Coord)
    reachableFrom c visited =
      if Set.member c cubes
      then (1, visited)
      else
        foldr
          (\nb (reached, visited') -> first (+ reached) $ reachableFrom nb $ Set.insert c visited')
          (0, visited)
          $ filter (validNeighbour visited)
          $ neighbours c

    validNeighbour :: Set Coord -> Coord -> Bool
    validNeighbour visited c@(x, y, z) =
      x `within` xBounds && y `within` yBounds && z `within` zBounds && not (Set.member c visited)

    [xBounds, yBounds, zBounds] = (subtract 1 . Set.findMin &&& (+ 1) . Set.findMax)
      . flip Set.map cubes <$> [fst3, snd3, thd3]

neighbours :: Coord -> [Coord]
neighbours c@(x, y, z) = filter ((== 1) . distance c)
  [(x', y', z') | x' <- adj x, y' <- adj y, z' <- adj z]
  where
    adj a = [a - 1 .. a + 1]

distance :: Coord -> Coord -> Int
distance (x1, y1, z1) (x2, y2, z2) = sum $ abs <$> [x2 - x1, y2 - y1, z2 - z1]

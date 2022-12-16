module Day15 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text ((<?>))
import Parsing (linesOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Utils (manhattanDistance)

type Coord = (Int, Int)

data Scan = Scan
 { sensors :: Map Coord Int
 , beacons :: Set Coord
 } deriving (Show)

instance Semigroup Scan where
  scan1 <> scan2 = Scan (sensors scan1 <> sensors scan2) (beacons scan1 <> beacons scan2)

instance Monoid Scan where
  mempty = Scan mempty mempty

parse :: Text -> Either String Scan
parse = P.parseOnly $ mconcat <$> linesOf do
  sensor <- (,) <$ P.string "Sensor at x=" <*> n <* P.string ", y=" <*> n <?> "sensor"
  beacon <- (,) <$ P.string ": closest beacon is at x=" <*> n <* P.string ", y=" <*> n <?> "beacon"
  return Scan
    { sensors = Map.singleton sensor $ manhattanDistance sensor beacon
    , beacons = Set.singleton beacon
    }
  where
    n :: P.Parser Int
    n = P.signed P.decimal

solve1 :: Scan -> Int
solve1 = undefined

solve2 :: Scan -> Int
solve2 = undefined

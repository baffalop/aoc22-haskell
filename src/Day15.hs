module Day15 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text ((<?>))
import Parsing (linesOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Interval (Interval, (<=..<=), Extended(..))
import qualified Data.Interval as I
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IS
import Utils (manhattanDistance, (<.>), within)
import Data.Function.Syntax ((.:))
import Safe (headMay)
import Data.Tuple.Extra (both)

type Coord = (Integer, Integer)

data Scan = Scan
 { sensors :: Map Coord Integer
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
    n :: Integral i => P.Parser i
    n = P.signed P.decimal

solve1 :: Scan -> Integer
solve1 Scan{ sensors, beacons } = visible - beaconCount
  where
    visible = positionsIn $ visibleOn y sensors
    beaconCount = toInteger $ Set.size $ Set.filter ((== y) . snd) beacons
    y = 2000000

solve2 :: Scan -> Maybe Integer
solve2 = uncurry (*) <.> headMay
  . Set.toList . Set.filter (uncurry (&&) . both (`within` limits))
  . Map.foldrWithKey (Set.intersection .: perimeter) Set.empty . sensors
  where
    limits = (0, 4000000)

perimeter :: Coord -> Integer -> Set Coord
perimeter (x, y) ((+ 1) -> range) =
  Set.fromList $ flip foldMap [-range..range] \offset ->
    [(x + offset, y + (range - offset)), (x + offset, y - (range - offset))]

visibleOn :: Integer -> Map Coord Integer -> IntervalSet Integer
visibleOn y = Map.foldrWithKey (IS.insert .: seesOn y) IS.empty

seesOn :: Integer -> Coord -> Integer -> Interval Integer
seesOn y (sensorX, sensorY) range =
  let xRange = range - abs (sensorY - y) in
  Finite (sensorX - xRange) <=..<= Finite (sensorX + xRange)

positionsIn :: IntervalSet Integer -> Integer
positionsIn = sum . ((+ 1) . I.width <.> IS.toList)

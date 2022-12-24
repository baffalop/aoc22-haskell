module Day16 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Data.Map (Map)
import qualified Data.Map as Map
import Parsing (linesOf, alphaWord)
import Control.Monad (void, guard)
import Data.Maybe (fromMaybe)

type Valves = Map ValveKey Valve
type Plan = [ValveKey]

newtype ValveKey = Key Text deriving (Show, Eq, Ord)

data Valve = Valve
  { flow :: Int
  , tunnels :: [ValveKey]
  } deriving (Show)

parse :: Text -> Either String Valves
parse = P.parseOnly $ Map.fromList <$> linesOf do
  key <- Key <$ P.string "Valve " <*> alphaWord
  flow <- P.string " has flow rate=" *> P.decimal
  void $ P.string "; tunnels lead to valve" <* P.many' (P.char 's') <* P.string " "
  tunnels <- (Key <$> alphaWord) `P.sepBy` P.string ", "
  return (key, Valve{..})

solve1 :: Valves -> Int
solve1 = undefined

solve2 :: Valves -> Int
solve2 = undefined

scoreFromTime :: Int -> Valves -> Plan -> Int
scoreFromTime t valves = snd . flip foldl (t, 0) \(t', score) k ->
  fromMaybe (t' - 1, score) do
    flow <- flow <$> Map.lookup k valves
    guard $ flow > 0
    return (t' - 2, score + (t' - 2) * flow)

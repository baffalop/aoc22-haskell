module Day16 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Data.Map (Map)
import qualified Data.Map as Map
import Parsing (linesOf, alphaWord)
import Control.Monad (void)

type Valves = Map ValveKey Valve

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

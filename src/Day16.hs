module Day16 where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Parsing (linesOf, alphaWord)
import Control.Monad (void)
import Data.Maybe (fromMaybe, catMaybes)
import Safe (maximumByMay)
import Data.Function (on)
import Debug.Trace (traceShowId)

type Valves = Map ValveKey Valve

newtype ValveKey = Key Text deriving (Show, Eq, Ord)

data Step = Open | GoTo ValveKey deriving (Show, Eq)

data Valve = Valve
  { flow :: Int
  , tunnels :: [ValveKey]
  } deriving (Show)

data Plan = Plan
  { score :: Int
  , time :: Int
  , opened :: Set ValveKey
  } deriving (Show)

parse :: Text -> Either String Valves
parse = P.parseOnly $ Map.fromList <$> linesOf do
  key <- Key <$ P.string "Valve " <*> alphaWord
  flow <- P.string " has flow rate=" *> P.decimal <* P.string "; "
  void $ P.string "tunnel leads to valve " <|> P.string "tunnels lead to valves "
  tunnels <- (Key <$> alphaWord) `P.sepBy` P.string ", "
  return (key, Valve{..})

solve1 :: Valves -> Int
solve1 = undefined

solve2 :: Valves -> Int
solve2 = undefined

optimumTo :: Valves -> ValveKey -> ValveKey -> Int -> Set ValveKey -> Maybe Plan
optimumTo valves origin target t' opened =
  findOptimum Plan { score = 0, time = t', .. } Set.empty origin
  where
    findOptimum :: Plan -> Set ValveKey -> ValveKey -> Maybe Plan
    findOptimum plan@Plan{ time = 0 } _ _ = Just plan
    findOptimum plan@Plan{ score = score', time = t, .. } visited cur =
      let
        Valve{..} = fromMaybe (error $ "invalid valve " <> show cur) $ Map.lookup cur valves
        withOpenedValve =
          if flow /= 0 && not (Set.member cur opened)
            then (:) $ findOptimum Plan
              { opened = Set.insert cur opened
              , time = t - 1
              , score = score' + (flow * (t - 1))
              }
              visited
              cur
            else id
      in
      if cur == target && Set.member cur opened
        then traceShowId $ Just plan
        else maximumByMay (compare `on` ((*) <$> score <*> time))
          $ catMaybes
          $ withOpenedValve
          $ findOptimum plan{ time = t - 1 } (Set.insert cur visited)
          <$> filter (not . (`Set.member` visited)) tunnels

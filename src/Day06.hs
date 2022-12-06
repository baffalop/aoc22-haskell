module Day06 (parse, solve1, solve2) where

import Data.Text (Text, unpack)
import Data.List (tails, findIndex, nub)
import Data.Maybe (fromMaybe)

parse :: Text -> String
parse = unpack

solve1 :: String -> Int
solve1 = fromMaybe 0  . findPacket 4

solve2 :: String -> Int
solve2 = fromMaybe 0 . findPacket 14

findPacket :: Int -> String -> Maybe Int
findPacket window = fmap (+ window) . findIndex (allDistinct . take window) . tails

allDistinct :: Eq a => [a] -> Bool
allDistinct xs = length (nub xs) == length xs

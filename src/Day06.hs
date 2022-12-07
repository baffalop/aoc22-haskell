module Day06 (parse, solve1, solve2) where

import Data.Text (Text, unpack)
import Data.List (tails, findIndex, nub)
import Utils ((<.>))

parse :: Text -> String
parse = unpack

solve1 :: String -> Maybe Int
solve1 = findPacket 4

solve2 :: String -> Maybe Int
solve2 = findPacket 14

findPacket :: Int -> String -> Maybe Int
findPacket window = (+ window) <.> findIndex (allDistinct . take window) . tails

allDistinct :: Eq a => [a] -> Bool
allDistinct xs = length (nub xs) == length xs

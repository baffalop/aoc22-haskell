module Day10 (parse, solve1, solve2, nthEvery, signals, signalStrengths) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Parsing (linesOf, negatable)
import Control.Monad (join)
import Utils ((<.>))

type Input = [Maybe Int]

parse :: Text -> Either String Input
parse = P.parseOnly $ join <.> linesOf $ P.choice
  [ [Nothing] <$ P.string "noop"
  , (Nothing :) . (: []) . Just <$ P.string "addx " <*> negatable
  ]

solve1 :: Input -> Int
solve1 = sum . nthEvery 20 40 . signalStrengths

solve2 :: Input -> Int
solve2 = undefined

signalStrengths :: [Maybe Int] -> [Int]
signalStrengths = zipWith (*) [1..] . signals

signals :: [Maybe Int] -> [Int]
signals = scanl (flip $ maybe id (+)) 1

nthEvery :: Int -> Int -> [a] -> [a]
nthEvery start n = snd <.> filter (\(i, _) -> (i + start) `mod` n == 0) . zip [1..]

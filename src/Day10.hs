module Day10 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Parsing (linesOf, negatable)
import Control.Monad (join)
import Data.List.Extra (chunksOf)
import Utils ((<.>))

type Input = [Maybe Int]

parse :: Text -> Either String Input
parse = P.parseOnly $ join <.> linesOf $ P.choice
  [ [Nothing] <$ P.string "noop"
  , (Nothing :) . (: []) . Just <$ P.string "addx " <*> negatable
  ]

solve1 :: Input -> Int
solve1 = sum . (head <.> chunksOf 40) . drop 19 . signalStrengths

solve2 :: Input -> Int
solve2 = undefined

signalStrengths :: [Maybe Int] -> [Int]
signalStrengths = zipWith (*) [1..] . signals

signals :: [Maybe Int] -> [Int]
signals = scanl (flip $ maybe id (+)) 1

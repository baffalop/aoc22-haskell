module Day10 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Parsing (linesOf, negatable)
import Control.Monad (join)
import Data.List.Extra (chunksOf)
import Utils ((<.>))
import Control.Arrow ((>>>))
import Data.Matrix (Matrix)
import qualified Data.Matrix as Mx

type Input = [Maybe Int]

data Pixel = Black | White
instance Show Pixel where
  show Black = "."
  show White = "#"

parse :: Text -> Either String Input
parse = P.parseOnly $ join <.> linesOf $ P.choice
  [ [Nothing] <$ P.string "noop"
  , (Nothing :) . (: []) . Just <$ P.string "addx " <*> negatable
  ]

solve1 :: Input -> Int
solve1 = sum . (head <.> chunksOf 40) . drop 19 . signalStrengths

solve2 :: Input -> Matrix Pixel
solve2 = render . (draw <.> chunksOf 40) . signals
  where
    render :: [[Pixel]] -> Matrix Pixel
    render = Mx.fromLists

draw :: [Int] -> [Pixel]
draw = zip [0..] >>> fmap \(pos, sig) ->
  if pos `elem` [sig - 1, sig + 1] then White else Black

signals :: [Maybe Int] -> [Int]
signals = scanl (flip $ maybe id (+)) 1

signalStrengths :: [Maybe Int] -> [Int]
signalStrengths = zipWith (*) [1..] . signals

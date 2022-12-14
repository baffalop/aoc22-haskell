{-# LANGUAGE DeriveGeneric #-}

module Day10 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Parsing (linesOf)
import Control.Monad (join)
import Data.List.Extra (chunksOf)
import Utils ((<.>))
import Control.Arrow ((>>>))
import Data.Matrix (Matrix)
import qualified Data.Matrix as Mx
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

type Input = [Maybe Int]

data Pixel = Black | White deriving (Generic)

instance NFData Pixel
instance Show Pixel where
  show Black = "."
  show White = "#"

parse :: Text -> Either String Input
parse = P.parseOnly $ join <.> linesOf $ P.choice
  [ [Nothing] <$ P.string "noop"
  , do n <- P.string "addx " >> P.signed P.decimal
       pure [Nothing, Just n]
  ]

solve1 :: Input -> Int
solve1 = sum . (head <.> chunksOf 40) . drop 19 . signalStrengths

solve2 :: Input -> Matrix Pixel
solve2 = Mx.fromLists . (draw <.> chunksOf 40) . signals

draw :: [Int] -> [Pixel]
draw = zip [0..] >>> fmap \(pos, sig) ->
  if pos `elem` [sig - 1 .. sig + 1] then White else Black

signals :: [Maybe Int] -> [Int]
signals = scanl (flip $ maybe id (+)) 1

signalStrengths :: [Maybe Int] -> [Int]
signalStrengths = zipWith (*) [1..] . signals

module Day17 (parse, solve1, solve2) where

import Prelude hiding (drop, ceiling)
import Data.Text (Text, unpack)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bifunctor (first, second)
import Data.Maybe (fromMaybe)

data Gust = L | R deriving (Show)

type Coord = (Int, Int)

type Block = Set Coord

parse :: Text -> Either String [Gust]
parse = traverse gust . head . lines . unpack
  where
    gust :: Char -> Either String Gust
    gust '<' = Right L
    gust '>' = Right R
    gust c = Left $ "Not a gust: " <> [c]

solve1 :: [Gust] -> Int
solve1 gusts = (+ 1) $ ceiling $ iterateBlocks 2022 blocks (cycle gusts) Set.empty
  where
    iterateBlocks :: Int -> [Int -> Block] -> [Gust] -> Block -> Block
    iterateBlocks 0 _ _ fallen = fallen
    iterateBlocks _ [] _ _ = error "empty blocks - shouldn't they be infinite?"
    iterateBlocks n (mkBlock:mkBlocks) gusts fallen =
      let (newFallen, newGusts) = dropBlock fallen gusts $ mkBlock $ ceiling fallen + 3
      in iterateBlocks (n - 1) mkBlocks newGusts newFallen

solve2 :: [Gust] -> Int
solve2 = undefined

dropBlock :: Block -> [Gust] -> Block -> (Block, [Gust])
dropBlock fallen = drop
  where
    drop :: [Gust] -> Block -> (Block, [Gust])
    drop [] _ = error "empty gusts - shouldn't they be infinite?"
    drop (gust:gusts) block = case step fallen gust block of
      Left blocks -> (blocks, gusts)
      Right dropped -> drop gusts dropped

step :: Block -> Gust -> Block -> Either Block Block
step fallen gust = fall . blow
  where
    blow :: Block -> Block
    blow block =
      let
        (buffet, hitsSide) = case gust of
          L -> (subtract 1, hitsLeft)
          R -> ((+ 1), hitsRight)

        blown = Set.map (first buffet) block
      in
      if clashes blown || hitsSide blown then block else blown

    fall :: Block -> Either Block Block
    fall block = let dropped = lower block in
      if clashes dropped || hitsFloor dropped
      then Left $ Set.union block fallen
      else Right dropped

    clashes :: Block -> Bool
    clashes = not . Set.disjoint fallen

ceiling :: Block -> Int
ceiling = fromMaybe (-1) . Set.lookupMax . Set.map snd

lower :: Block -> Block
lower = Set.map $ second $ subtract 1

hitsFloor :: Block -> Bool
hitsFloor block = Set.findMin (Set.map snd block) < 0

hitsLeft :: Block -> Bool
hitsLeft block = Set.findMin (Set.map fst block) < 0

hitsRight :: Block -> Bool
hitsRight block = Set.findMax (Set.map fst block) > 6

blocks :: [Int -> Block]
blocks = cycle $ (Set.fromList .) <$>
  [ \y -> (,y) <$> [2..5]
  , \y -> [(3, y + 2)] <> ((,y + 1) <$> [2..4]) <> [(3, y)]
  , \y -> [(3, y + 2), (3, y + 1)] <> ((,y) <$> [2..4])
  , \y -> (2,) <$> [y .. y + 3]
  , \y -> [(x, y') | x <- [2, 3], y' <- [y, y + 1]]
  ]

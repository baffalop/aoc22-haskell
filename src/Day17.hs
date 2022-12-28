module Day17 (parse, solve1, solve2) where

import Prelude hiding (drop, ceiling)
import Data.Text (Text, unpack)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bifunctor (first, second)
import Data.Maybe (fromMaybe)

data Gust = L | R deriving (Show)

type Coord = (Int, Int)

newtype Block = Block (Set Coord)
newtype Rock = Rock (Set Coord)

parse :: Text -> Either String [Gust]
parse = traverse gust . head . lines . unpack
  where
    gust :: Char -> Either String Gust
    gust '<' = Right L
    gust '>' = Right R
    gust c = Left $ "Not a gust: " <> [c]

solve1 :: [Gust] -> Int
solve1 gusts' = (+ 1) $ ceiling $ iterateBlocks 2022 blocks (cycle gusts') (Rock Set.empty)
  where
    iterateBlocks :: Int -> [Int -> Block] -> [Gust] -> Rock -> Rock
    iterateBlocks 0 _ _ rock = rock
    iterateBlocks _ [] _ _ = error "empty blocks - shouldn't they be infinite?"
    iterateBlocks n (mkBlock:mkBlocks) gusts fallen =
      let (newRock, nextGusts) = dropBlock fallen gusts $ mkBlock $ ceiling fallen + 3
      in iterateBlocks (n - 1) mkBlocks nextGusts newRock

solve2 :: [Gust] -> Int
solve2 = undefined

dropBlock :: Rock -> [Gust] -> Block -> (Rock, [Gust])
dropBlock rock = drop
  where
    drop :: [Gust] -> Block -> (Rock, [Gust])
    drop [] _ = error "empty gusts - shouldn't they be infinite?"
    drop (gust:gusts) block = case step rock gust block of
      Left rock' -> (rock', gusts)
      Right dropped -> drop gusts dropped

step :: Rock -> Gust -> Block -> Either Rock Block
step rock gust = fall . blow
  where
    blow :: Block -> Block
    blow block =
      let
        (buffet, hitsSide) = case gust of
          L -> (subtract 1, hitsLeft)
          R -> ((+ 1), hitsRight)

        blown = move (first buffet) block
      in
      if clashes blown rock || hitsSide blown then block else blown

    fall :: Block -> Either Rock Block
    fall block = let dropped = lower block in
      if clashes dropped rock || hitsFloor dropped
      then Left $ calcify block rock
      else Right dropped

clashes :: Block -> Rock -> Bool
clashes (Block block) (Rock rock) = not $ Set.disjoint rock block

ceiling :: Rock -> Int
ceiling (Rock r) = fromMaybe (-1) $ Set.lookupMax $ Set.map snd r

move :: (Coord -> Coord) -> Block -> Block
move f (Block block) = Block $ Set.map f block

calcify :: Block -> Rock -> Rock
calcify (Block block) (Rock rock) = Rock $ Set.union rock block

lower :: Block -> Block
lower = move $ second $ subtract 1

hitsFloor :: Block -> Bool
hitsFloor (Block block) = Set.findMin (Set.map snd block) < 0

hitsLeft :: Block -> Bool
hitsLeft (Block block) = Set.findMin (Set.map fst block) < 0

hitsRight :: Block -> Bool
hitsRight (Block block) = Set.findMax (Set.map fst block) > 6

blocks :: [Int -> Block]
blocks = cycle $ fmap (Block . Set.fromList) <$>
  [ \y -> (,y) <$> [2..5]
  , \y -> [(3, y + 2)] <> ((,y + 1) <$> [2..4]) <> [(3, y)]
  , \y -> [(3, y + 2), (3, y + 1)] <> ((,y) <$> [2..4])
  , \y -> (2,) <$> [y .. y + 3]
  , \y -> [(x, y') | x <- [2, 3], y' <- [y, y + 1]]
  ]

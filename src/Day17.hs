{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Day17 (parse, solve1, solve2, viz) where

import Prelude hiding (drop)
import Data.Text (Text, unpack)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Data.Matrix (Matrix)
import qualified Data.Matrix as Mx
import Data.Bifunctor (first, second)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Control.Arrow ((>>>), (***))
import Control.Monad (replicateM_, (>=>))
import Lens.Micro.Platform (Lens', makeLensesFor, (^.), (%~))
import Data.Functor ((<&>))
-- import Debug.Trace (traceShowM)

data Gust = L | R deriving (Show)

type Coord = (Int, Int)

newtype Block = Block (Set Coord) deriving (Show, Eq)
newtype Rock = Rock (Set Coord) deriving (Show, Eq)

data Cave = Cave
  { rock :: Rock
  , mkBlocks :: [Int -> Block]
  , gusts :: [Gust]
  }

makeLensesFor [("rock", "_rock"), ("mkBlocks", "_blocks"), ("gusts", "_gusts")] ''Cave

parse :: Text -> Either String [Gust]
parse = traverse gust . head . lines . unpack
  where
    gust :: Char -> Either String Gust
    gust '<' = Right L
    gust '>' = Right R
    gust c = Left $ "Not a gust: " <> [c]

solve1 :: [Gust] -> Int
solve1 = sky . rock . State.execState (replicateM_ 20 releaseBlock) . initCave

solve2 :: [Gust] -> [Int]
solve2 = State.evalState findLoopss . initCave
  where
    findLoopss :: State Cave [Int]
    findLoopss = do
      n <- findLoop 1 Set.empty
      r <- State.gets rock
      findLoops r 1 5

    findLoops :: Rock -> Int -> Int -> State Cave [Int]
    findLoops _ _ 0 = return []
    findLoops r n m = do
      rock <- releaseBlock
      if r == rock
        then (n:) <$> findLoops r 1 (m - 1)
        else findLoops r (n + 1) m

    findLoop :: Int -> Set (Set Coord) -> State Cave Int
    findLoop n prev = do
      Rock r <- releaseBlock
      if Set.member r prev
        then return n
        else findLoop (n + 1) $ Set.insert r prev

releaseBlock :: State Cave Rock
releaseBlock = do
  fallen <- State.gets rock
  mkBlock <- consume _blocks
  dropBlock $ mkBlock $ sky fallen + 3
  State.modify $ _rock %~ crop
  -- traceShowM $ viz (Block mempty) fallen
  State.gets rock

dropBlock :: Block -> State Cave ()
dropBlock = blow >=> \block -> do
  let dropped = lower block
  rock <- State.gets rock
  -- traceShowM $ viz block rock
  if clashes dropped rock || hitsFloor dropped
    then State.modify $ _rock %~ calcify block
    else dropBlock dropped

blow :: Block -> State Cave Block
blow block = do
  rock <- State.gets rock
  gust <- consume _gusts
  let
    (buffet, hitsSide) = case gust of
      L -> (subtract 1, hitsLeft)
      R -> ((+ 1), hitsRight)

    blown = move (first buffet) block
  if clashes blown rock || hitsSide blown
    then return block
    else return blown

clashes :: Block -> Rock -> Bool
clashes (Block block) (Rock rock) = not $ Set.disjoint rock block

sky :: Rock -> Int
sky (Rock r) = maybe 0 (+ 1) $ Set.lookupMax $ Set.map snd r

ground :: Rock -> Int
ground (Rock r) = minimum $ [0..6] <&> fromMaybe 0
  . Set.lookupMax . Set.map snd . \x -> Set.filter ((== x) . fst) r

crop :: Rock -> Rock
crop rock@(Rock r) = Rock $ Set.filter ((>= ground rock) . snd) r

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

consume :: Lens' s [a] -> State s a
consume l = do
  x <- State.gets $ head . (^. l)
  State.modify $ l %~ tail
  return x

blocks :: [Int -> Block]
blocks = cycle $ fmap (Block . Set.fromList) <$>
  [ \y -> (,y) <$> [2..5]
  , \y -> [(3, y + 2)] <> ((,y + 1) <$> [2..4]) <> [(3, y)]
  , \y -> [(4, y + 2), (4, y + 1)] <> ((,y) <$> [2..4])
  , \y -> (2,) <$> [y .. y + 3]
  , \y -> [(x, y') | x <- [2, 3], y' <- [y, y + 1]]
  ]

initCave :: [Gust] -> Cave
initCave gusts' = Cave
  { rock = Rock Set.empty
  , mkBlocks = blocks
  , gusts = cycle gusts'
  }

data Sq = BlockSq | RockSq | Empty
instance Show Sq where
  show BlockSq = "@"
  show RockSq = "#"
  show Empty = "."

viz :: Block -> Rock -> Matrix Sq
viz b@(Block block) r@(Rock rock) =
  Mx.matrix height 7 $ swap >>> (subtract 1 *** (ceil -)) >>> \c ->
    if Set.member c block then BlockSq
    else if Set.member c rock then RockSq
    else Empty
  where
    height = max 1 $ ceil - ground glom
    ceil = sky glom
    glom = calcify b r

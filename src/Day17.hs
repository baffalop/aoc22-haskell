{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Day17 (parse, solve1, solve2, viz) where

import Prelude hiding (drop)
import Data.Text (Text, unpack)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Data.Matrix (Matrix)
import qualified Data.Matrix as Mx
import Data.Bifunctor (first, second)
import Data.Tuple (swap)
import Control.Arrow ((>>>), (***))
import Control.Monad (replicateM_, (>=>))
import Lens.Micro.Platform (Lens', makeLensesFor, (^.), (%~), (.~), (+~))
import Data.Functor ((<&>))
import Debug.Trace (traceShowM)

data Gust = L | R deriving (Show, Eq, Ord)

type Coord = (Int, Int)

newtype Block = Block (Set Coord) deriving (Show, Eq, Ord)
newtype Rock = Rock (Set Coord) deriving (Show, Eq, Ord)

data Cave = Cave
  { rock :: Rock
  , ground :: Int
  , mkBlocks :: [Int -> Block]
  , gusts :: [Gust]
  }

makeLensesFor [("rock", "_rock"), ("ground", "_ground"), ("mkBlocks", "_blocks"), ("gusts", "_gusts")] ''Cave

parse :: Text -> Either String [Gust]
parse = traverse gust . head . lines . unpack
  where
    gust :: Char -> Either String Gust
    gust '<' = Right L
    gust '>' = Right R
    gust c = Left $ "Not a gust: " <> [c]

solve1 :: [Gust] -> Int
solve1 = totalHeight . State.execState (replicateM_ 2022 releaseBlock) . initCave

solve2 :: [Gust] -> Integer
solve2 gusts' = State.evalState solve $ initCave gusts'
  where
    solve :: State Cave Integer
    solve = do
      (spentBlocks, loop, loopHeight) <- findLoop 1 Map.empty
      let (loops, remainder) = (1000000000000 - spentBlocks) `divMod` loop
      replicateM_ (fromInteger remainder) releaseBlock
      remHeight <- State.gets $ toInteger . sky . rock
      return $ (loops * loopHeight) + remHeight

    findLoop :: Integer
      -> Map (Rock, Block, [Gust]) (Integer, Integer)
      -> State Cave (Integer, Integer, Integer)
    findLoop n prev = do
      curRock <- releaseBlock
      mkBlock <- State.gets $ head . mkBlocks
      gusts <- State.gets $ take gustLength . gusts
      height <- State.gets $ toInteger . totalHeight
      let state = (curRock, mkBlock 0, gusts)
      case Map.lookup state prev of
        Nothing ->
          findLoop (n + 1) $ Map.insert state (n, height) prev
        Just (initBlocks, initHeight) ->
          return (n, n - initBlocks, height - initHeight)

    gustLength :: Int
    gustLength = length gusts'

releaseBlock :: State Cave Rock
releaseBlock = do
  fallen <- State.gets rock
  mkBlock <- consume _blocks
  dropBlock $ mkBlock $ sky fallen + 3
  cropRock
  -- traceShowRock
  -- traceShowM =<< State.gets ground
  State.gets rock

dropBlock :: Block -> State Cave ()
dropBlock = blow >=> \block -> do
  let dropped = lower block
  rock <- State.gets rock
  -- traceShowM $ viz block rock
  if clashes dropped rock || hitsGround dropped
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

cropRock :: State Cave ()
cropRock = do
  rock@(Rock r) <- State.gets rock
  let ground = groundOf rock
  let cropped = Rock $ Set.map (second $ subtract ground) $ Set.filter ((>= ground) . snd) r
  State.modify $ (_rock .~ cropped) . (_ground +~ ground)

traceShowRock :: State Cave ()
traceShowRock = traceShowM . viz (Block Set.empty) =<< State.gets rock

clashes :: Block -> Rock -> Bool
clashes (Block block) (Rock rock) = not $ Set.disjoint rock block

totalHeight :: Cave -> Int
totalHeight Cave{ rock, ground } = ground + sky rock

sky :: Rock -> Int
sky (Rock r) = maybe 0 (+ 1) $ Set.lookupMax $ Set.map snd r

groundOf :: Rock -> Int
groundOf (Rock r) = minimum $ [0..6] <&> maybe 0 (+ 1)
  . Set.lookupMax . Set.map snd . \x -> Set.filter ((== x) . fst) r

move :: (Coord -> Coord) -> Block -> Block
move f (Block block) = Block $ Set.map f block

calcify :: Block -> Rock -> Rock
calcify (Block block) (Rock rock) = Rock $ Set.union rock block

lower :: Block -> Block
lower = move $ second $ subtract 1

hitsGround :: Block -> Bool
hitsGround (Block block) = Set.findMin (Set.map snd block) < 0

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
  , ground = 0
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
  Mx.matrix height 7 $ swap >>> (subtract 1 *** (height -)) >>> \c ->
    if Set.member c block then BlockSq
    else if Set.member c rock then RockSq
    else Empty
  where
    height = max 1 $ sky $ calcify b r

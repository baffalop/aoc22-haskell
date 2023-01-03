{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Day17 (parse, solve1, solve2, traceShowRock) where

import Data.Text (Text, unpack)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State (State, execState, evalState)
import qualified Control.Monad.State as State
import Data.Matrix (Matrix)
import qualified Data.Matrix as Mx
import Data.Bifunctor (first, second)
import Data.Tuple (swap)
import Control.Arrow ((>>>), (***))
import Control.Monad (replicateM_, (>=>))
import Lens.Micro.Platform (Lens', makeLensesFor, (%=), (+=), (^.), (<~))
import Debug.Trace (traceShowM)
import Utils (within)

data Gust = L | R deriving (Show, Eq, Ord)

type Coord = (Int, Int)

newtype Block = Block (Set Coord) deriving (Show, Eq, Ord)
newtype Rock = Rock { rocks :: Set Coord } deriving (Eq, Ord)

data Cave = Cave
  { rock :: Rock
  , ground :: Int
  , mkBlocks :: [Int -> Block]
  , gusts :: [Gust]
  }

makeLensesFor [("rock", "_rock"), ("ground", "_ground"), ("mkBlocks", "_blocks"), ("gusts", "_gusts")] ''Cave
makeLensesFor [("rocks", "_rocks")] ''Rock

parse :: Text -> Either String [Gust]
parse = traverse gust . head . lines . unpack
  where
    gust :: Char -> Either String Gust
    gust '<' = Right L
    gust '>' = Right R
    gust c = Left $ "Not a gust: " <> [c]

solve1 :: [Gust] -> Int
solve1 = totalHeight . execState (replicateM_ 2022 releaseBlock) . initCave

solve2 :: [Gust] -> Integer
solve2 gusts' = evalState (solveFor 1000000000000) $ initCave gusts'
  where
    solveFor :: Integer -> State Cave Integer
    solveFor target = do
      (spentBlocks, loop, loopHeight) <- findLoop 1 Map.empty
      let (loops, remainder) = (target - spentBlocks) `divMod` loop
      replicateM_ (fromInteger remainder) releaseBlock
      remHeight <- State.gets $ toInteger . totalHeight
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
  dropBlock $ mkBlock $ heightOf fallen + 3
  cropRock
  -- traceShowRock
  -- traceShowM =<< State.gets ground
  State.gets rock

dropBlock :: Block -> State Cave ()
dropBlock = blow >=> \block -> do
  let dropped = move (second $ subtract 1) block
  rock <- State.gets rock
  -- traceShowM $ viz block rock
  if clashes dropped rock || hitsGround dropped
    then _rock %= calcify block
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
  _rock %= reachable
  ground <- State.gets $ groundOf . rock
  _rock . _rocks %= Set.map (second $ subtract ground)
  _ground += ground

reachable :: Rock -> Rock
reachable rock@Rock{ rocks } = Rock $ fst $ reachableFrom (0, height) Set.empty
  where
    reachableFrom :: Coord -> Set Coord -> (Set Coord, Set Coord)
    reachableFrom c (Set.insert c -> visited) =
      if Set.member c rocks
      then (Set.singleton c, visited)
      else
        foldr
          (\nb (reached, visited') -> first (reached <>) $ reachableFrom nb visited')
          (Set.empty, visited)
          $ neighbours visited c

    neighbours :: Set Coord -> Coord -> [Coord]
    neighbours visited (x, y) = filter (validNeighbour visited)
      [(x', y') | x' <- [x - 1 .. x + 1], y' <- [y - 1 .. y + 1]]

    validNeighbour :: Set Coord -> Coord -> Bool
    validNeighbour visited c@(x, y) =
      x `within` (0, 6) && y `within` (0, height) && not (Set.member c visited)

    height = heightOf rock

traceShowRock :: State Cave ()
traceShowRock = traceShowM =<< State.gets rock

clashes :: Block -> Rock -> Bool
clashes (Block block) (Rock rock) = not $ Set.disjoint rock block

totalHeight :: Cave -> Int
totalHeight Cave{ rock, ground } = ground + heightOf rock

heightOf :: Rock -> Int
heightOf (Rock r) = maybe 0 (+ 1) $ Set.lookupMax $ Set.map snd r

groundOf :: Rock -> Int
groundOf (Rock r) = Set.findMin (Set.map snd r)

move :: (Coord -> Coord) -> Block -> Block
move f (Block block) = Block $ Set.map f block

calcify :: Block -> Rock -> Rock
calcify (Block block) (Rock rock) = Rock $ Set.union rock block

hitsGround :: Block -> Bool
hitsGround (Block block) = Set.findMin (Set.map snd block) < 0

hitsLeft :: Block -> Bool
hitsLeft (Block block) = Set.findMin (Set.map fst block) < 0

hitsRight :: Block -> Bool
hitsRight (Block block) = Set.findMax (Set.map fst block) > 6

consume :: Lens' s [a] -> State s a
consume l = do
  x <- State.gets $ head . (^. l)
  l %= tail
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

instance Show Rock where show = show . viz (Block Set.empty)

viz :: Block -> Rock -> Matrix Sq
viz b@(Block block) r@(Rock rock) =
  Mx.matrix height 7 $ swap >>> (subtract 1 *** (height -)) >>> \c ->
    if Set.member c block then BlockSq
    else if Set.member c rock then RockSq
    else Empty
  where
    height = max 1 $ heightOf $ calcify b r

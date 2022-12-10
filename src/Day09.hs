module Day09 (parse, solve1, solve2) where

import Prelude hiding (head, tail)
import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Parsing (linesOf)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (join)
import Data.Foldable (foldl')
import Data.Tuple.Extra (both)

newtype Vec = Vec (Int, Int) deriving (Show, Eq)
newtype Pos = Pos (Int, Int) deriving (Show, Eq, Ord)

type Input = [Vec]

parse :: Text -> Either String Input
parse = P.parseOnly $ join <$> linesOf move
  where
    move :: P.Parser [Vec]
    move = flip replicate <$> P.choice
      [ Vec (1, 0) <$ P.char 'R'
      , Vec (-1, 0) <$ P.char 'L'
      , Vec (0, 1) <$ P.char 'U'
      , Vec (0, -1) <$ P.char 'D'
      ]
      <* P.space <*> P.decimal

solve1 :: Input -> Int
solve1 = moveRopeOfLength 2

solve2 :: Input -> Int
solve2 = moveRopeOfLength 10

moveRopeOfLength :: Int -> Input -> Int
moveRopeOfLength ropeLength = Set.size . snd . foldl' (flip applyMove) initial
  where
    applyMove :: Vec -> ([Pos], Set Pos) -> ([Pos], Set Pos)
    applyMove v (moveRope v -> rope, trail) = (rope, Set.insert (last rope) trail)

    initial :: ([Pos], Set Pos)
    initial = (replicate ropeLength origin, Set.singleton origin)

moveRope :: Vec -> [Pos] -> [Pos]
moveRope v = \case
  ((moveBy v -> head):tail@(neck:_)) ->
    let nextMove = neck `follow` head in
    head : if nextMove == zero then tail else moveRope nextMove tail
  rope -> moveBy v <$> rope

follow :: Pos -> Pos -> Vec
follow follower target =
  let (Vec vec@(vx, vy)) = vecFrom follower target in
  if abs vx > 1 || abs vy > 1 then Vec (both signum vec) else zero

moveBy :: Vec -> Pos -> Pos
moveBy (Vec (vx, vy)) (Pos (x, y)) = Pos (x + vx, y + vy)

vecFrom :: Pos -> Pos -> Vec
vecFrom (Pos (x1, y1)) (Pos (x2, y2)) = Vec (x2 - x1, y2 - y1)

zero :: Vec
zero = Vec (0, 0)

origin :: Pos
origin = Pos (0, 0)

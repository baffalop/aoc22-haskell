module Day09 (parse, solve1, solve2) where

import Data.Text (Text, unpack)
import Safe (readMay)
import Data.Either.Extra (maybeToEither)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (join)
import Data.Foldable (foldl')
import Data.Tuple.Extra (both)
import Data.Function ((&))
import Utils ((<.>))

newtype Vec = Vec (Int, Int) deriving (Show, Eq)
newtype Pos = Pos (Int, Int) deriving (Show, Eq, Ord)

type Input = [Vec]

parse :: Text -> Either String Input
parse = join <.> traverse parseMove . lines . unpack
  where
    parseMove :: String -> Either String [Vec]
    parseMove (words -> [dir, countStr]) = do
      count <- readMay countStr & maybeToEither ("Not an int: " <> countStr)
      vec <- case dir of
        "R" -> Right $ Vec (1, 0)
        "L" -> Right $ Vec (-1, 0)
        "U" -> Right $ Vec (0, 1)
        "D" -> Right $ Vec (0, -1)
        d -> Left $ "Not a direction: " <> d
      pure $ replicate count vec
    parseMove str = Left $ "Not two words: " <> str

solve1 :: Input -> Int
solve1 = solveFor 2

solve2 :: Input -> Int
solve2 = solveFor 10

solveFor :: Int -> Input -> Int
solveFor ropeLength = Set.size . snd . foldl' applyMove initial
  where
    applyMove :: ([Pos], Set Pos) -> Vec -> ([Pos], Set Pos)
    applyMove (rope, trail) move =
      let moved = moveRope move rope in
      (moved, Set.insert (last moved) trail)

    initial :: ([Pos], Set Pos)
    initial = (replicate ropeLength origin, Set.singleton origin)

moveRope :: Vec -> [Pos] -> [Pos]
moveRope v = \case
  (head:tail@(neck:_)) ->
    let
      newHead = head & moveBy v
      nextMove = neck `follow` newHead
    in
    newHead : if nextMove == zero then tail else moveRope nextMove tail
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
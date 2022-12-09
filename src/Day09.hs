module Day09 (parse, solve1, solve2, vectorFrom, moveBy, follow, Pos(..), Vec(..)) where

import Data.Text (Text, unpack)
import Safe (readMay)
import Data.Either.Extra (maybeToEither)
import Data.Function ((&))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (both, thd3)
import Control.Monad (join)
import Data.Foldable (foldl')

newtype Vec = Vec (Int, Int) deriving (Show)
newtype Pos = Pos (Int, Int) deriving (Show, Eq, Ord)

type Input = [[Vec]]

parse :: Text -> Either String Input
parse = traverse parseMove . lines . unpack
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
solve1 = Set.size . thd3 . foldl' move initial . join
  where
    move :: (Pos, Pos, Set Pos) -> Vec -> (Pos, Pos, Set Pos)
    move (head, tail, trail) v =
      let
        newHead = head `moveBy` v
        newTail = tail `follow` newHead
      in
      (newHead, newTail, Set.insert newTail trail)

solve2 :: Input -> Int
solve2 = undefined

initial :: (Pos, Pos, Set Pos)
initial = (Pos (0, 0), Pos (0, 0), Set.singleton $ Pos (0, 0))

follow :: Pos -> Pos -> Pos
follow follower target =
  let (Vec vec@(vx, vy)) = vectorFrom follower target in
  if abs vx < 2 && abs vy < 2 then follower
  else follower `moveBy` Vec (both signum vec)

moveBy :: Pos -> Vec -> Pos
moveBy (Pos (x, y)) (Vec (vx, vy)) = Pos (x + vx, y + vy)

vectorFrom :: Pos -> Pos -> Vec
vectorFrom (Pos (x1, y1)) (Pos (x2, y2)) =
  Vec (x2 - x1, y2 - y1)

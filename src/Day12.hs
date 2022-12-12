module Day12 (parse, solve1, solve2) where

import Prelude hiding (map)
import Data.Text (Text, unpack)
import Data.Char (ord)
import Data.Matrix (Matrix)
import qualified Data.Matrix as Mx
import Data.Either.Extra (maybeToEither)
import Data.Function ((&))
import Data.List (elemIndex)
import Data.Maybe (isJust)
import Data.Tuple.Extra (both)
import Utils (indexedFind)

type Terrain = Matrix Int
type Coord = (Int, Int)

data Hill = Hill
  { terrain :: Terrain
  , start :: Coord
  , end :: Coord
  } deriving (Show)

parse :: Text -> Either String Hill
parse (lines . unpack -> map) = do
  start <- findCoord 'S' & maybeToEither "Could not find start"
  end <- findCoord 'E' & maybeToEither "Could not find end"
  let terrain = fmap ord $ Mx.setElem 'a' start $ Mx.setElem 'z' end $ Mx.fromLists map
  pure $ Hill { .. }
  where
    findCoord :: Char -> Maybe Coord
    findCoord c = do
      (y, maybeX) <- indexedFind isJust $ elemIndex c <$> map
      x <- maybeX
      pure $ both (+ 1) (y, x)

solve1 :: Hill -> Int
solve1 = undefined

solve2 :: Hill -> Int
solve2 = undefined

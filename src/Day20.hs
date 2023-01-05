module Day20 (parse, solve1, solve2) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text, unpack)
import Utils ((<.>))

data Mixing = Unmixed | Mixed

data ZipV a = ZipV (Vector a) (Vector a)
  deriving (Show)

type Input = Vector Int

parse :: Text -> Input
parse = read <.> V.fromList . lines . unpack

solve1 :: Input -> Input
solve1 = mix

solve2 :: Input -> Input
solve2 = undefined

mix :: Input -> Input
mix xs = remix $ zipV $ (Unmixed,) <$> xs
  where
    len :: Int
    len = length xs

    remix :: ZipV (Mixing, Int) -> Input
    remix z = case unconsi z of
      Left done -> snd <$> done
      Right (x@(Mixed, _), _, remainder) -> remix $ x `putBehind` remainder
      Right ((Unmixed, x), i, remainder) -> remix $ insertZ (i + x `mod` len - 1) (Mixed, x) remainder

zipV :: Vector a -> ZipV a
zipV = ZipV V.empty

unconsi :: ZipV a -> Either (Vector a) (a, Int, ZipV a)
unconsi (ZipV behind ahead) = case V.uncons ahead of
  Nothing -> Left behind
  Just (x, following) -> Right (x, length behind, ZipV behind following)

putBehind :: a -> ZipV a -> ZipV a
putBehind x (ZipV behind ahead) = ZipV (behind <> V.singleton x) ahead

insertZ :: Int -> a -> ZipV a -> ZipV a
insertZ i x (ZipV behind ahead) = case length behind of
  len | i < len -> ZipV (insertV i x behind) ahead
      | otherwise -> ZipV behind $ insertV (i - len) x ahead

insertV :: Int -> a -> Vector a -> Vector a
insertV i x v = before <> V.singleton x <> after
  where (before, after) = V.splitAt i v

module Day20 (parse, solve1, solve2) where

import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Text (Text, unpack)
import Utils ((<.>))
import Data.Maybe (fromMaybe)

type Input = Vector Int

data ZipV a = ZipV (Vector a) (Vector a) deriving (Show)

data Mixing = Unmixed | Mixed deriving (Show)

parse :: Text -> Input
parse = read <.> V.fromList . lines . unpack

solve1 :: Input -> Int
solve1 = sum . coords . mix
  where
    coords :: Input -> [Int]
    coords xs =
      let i0 = fromMaybe 0 $ V.findIndex (== 0) xs in
      (xs !) . (`mod` length xs) . (i0 +) <$> [1000, 2000, 3000]

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
      Right ((Unmixed, x), i, remainder) -> remix $ insertZ ((i + x) `mod` (len - 1)) (Mixed, x) remainder

unconsi :: ZipV a -> Either (Vector a) (a, Int, ZipV a)
unconsi (ZipV behind ahead) = case V.uncons ahead of
  Nothing -> Left behind
  Just (x, following) -> Right (x, length behind, ZipV behind following)

putBehind :: a -> ZipV a -> ZipV a
putBehind x (ZipV behind ahead) = ZipV (behind <> V.singleton x) ahead

insertZ :: Int -> a -> ZipV a -> ZipV a
insertZ i x (ZipV behind ahead) = case length behind of
  len | i == 0 -> ZipV behind $ ahead <> V.singleton x
      | i <= len -> ZipV (insertV i x behind) ahead
      | otherwise -> ZipV behind $ insertV (i - len) x ahead

insertV :: Int -> a -> Vector a -> Vector a
insertV i x v = before <> V.singleton x <> after
  where (before, after) = V.splitAt i v

zipV :: Vector a -> ZipV a
zipV = ZipV V.empty

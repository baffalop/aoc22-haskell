{-# LANGUAGE InstanceSigs #-}

module Day13 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Parsing (pairBy)
import Data.List (findIndices, sort)
import Utils ((<.>))

type Packet = Nested Int
type Input = [(Packet, Packet)]

data Nested a = One a | Some [Nested a] deriving (Show, Eq)

instance Ord a => Ord (Nested a) where
  compare :: Nested a -> Nested a -> Ordering
  compare = curry $ \case
    (One a, One b) -> compare a b
    (One a, bs) -> compare (Some [One a]) bs
    (as, One b) -> compare as (Some [One b])
    (Some [], Some []) -> EQ
    (Some [], _) -> LT
    (_, Some []) -> GT
    (Some (a:as), Some (b:bs)) ->
      if a == b then compare (Some as) (Some bs) else compare a b

parse :: Text -> Either String Input
parse = P.parseOnly $
  pairBy '\n' nested `P.sepBy` (P.endOfLine <* P.endOfLine)
  where
    nested :: P.Parser Packet
    nested = P.choice
      [ One <$> P.decimal :: P.Parser Packet
      , Some <$ P.char '[' <*> nested `P.sepBy` P.char ',' <* P.char ']'
      ]

solve1 :: Input -> Int
solve1 = sum . indicesNat (uncurry (<))

solve2 :: Input -> Int
solve2 = product . indicesNat (`elem` dividers) . sort . (dividers <>) . foldMap unpair
  where
    dividers = [Some [Some [One 2]], Some [Some [One 6]]]

indicesNat :: (a -> Bool) -> [a] -> [Int]
indicesNat f = (+ 1) <.> findIndices f

unpair :: (a, a) -> [a]
unpair (a, b) = [a, b]

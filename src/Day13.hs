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
  compare = curry $ \case
    (One a, One b) -> compare a b
    (One a, Some bs) -> compare [One a] bs
    (Some as, One b) -> compare as [One b]
    (Some as, Some bs) -> compare as bs

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

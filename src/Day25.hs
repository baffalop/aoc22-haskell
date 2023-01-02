{-# LANGUAGE DeriveGeneric #-}

module Day25 (parse, solve1, solve2) where

import Data.Text (Text)
import Data.Attoparsec.Text (parseOnly)
import qualified Data.Attoparsec.Text as P
import Parsing (linesOf)
import Utils ((<.>))
import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

newtype Snafu = Snafu [Snafit] deriving (Eq, Generic)
newtype Snafit = S Int deriving (Eq, Generic)

instance Show Snafit where
  show (S (-2)) = "="
  show (S (-1)) = "-"
  show (S d) = show d

instance Show Snafu where
  show (Snafu s) = foldMap show $ reverse s

instance Semigroup Snafu where
  Snafu s1 <> Snafu s2 = Snafu $ add s1 s2
    where
      add (S x : xs) (S y : ys) =
        let
          (snafit, carry) = case x + y of
            r | r < -2 -> (r + 5, [S (-1)])
              | r > 2 -> (r - 5, [S 1])
              | otherwise -> (r, [])
        in S snafit : carry `add` xs `add` ys

      add xs ys = xs <|> ys

instance Monoid Snafu where
  mempty = Snafu []

instance NFData Snafit
instance NFData Snafu

parse :: Text -> Either String [Snafu]
parse = parseOnly $ linesOf $ Snafu . reverse <.> P.many1 $ P.choice
  [ S (-1) <$ P.char '-'
  , S (-2) <$ P.char '='
  , S . read . (:[]) <$> P.digit
  ]

solve1 :: [Snafu] -> Snafu
solve1 = mconcat

solve2 :: [Snafu] -> String
solve2 = undefined

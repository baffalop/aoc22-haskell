module Day17 (parse, solve1, solve2) where

import Data.Text (Text, unpack)

data Gust = L | R deriving (Show)

parse :: Text -> Either String [Gust]
parse = traverse gust . head . lines . unpack
  where
    gust :: Char -> Either String Gust
    gust '<' = Right L
    gust '>' = Right R
    gust c = Left $ "Not a gust: " <> [c]

solve1 :: [Gust] -> Int
solve1 = undefined

solve2 :: [Gust] -> Int
solve2 = undefined

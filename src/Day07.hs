module Day07 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser)
import Parsing (linesOf, word, alphaWord)
import Control.Applicative ((<|>))
import Data.Maybe (catMaybes)
import Data.List (sort)

type FS = [FsItem]

data FsItem
  = File !Size !Name
  | Dir !Size !Name FS
  deriving (Show)

type Size = Int
type Name = Text

parse :: Text -> Either String FsItem
parse = P.parseOnly recursedDir
  where
    recursedDir :: Parser FsItem
    recursedDir = mkDir
      <$ P.string "$ cd " <*> dirname <* P.endOfLine
      <* P.string "$ ls" <* P.endOfLine
      <*> fs <* (P.endOfLine <|> P.endOfInput)
      <* (() <$ P.string "$ cd .." <|> P.endOfInput)

    fs :: Parser FS
    fs = catMaybes <$>
      linesOf (Just <$> file <|> Nothing <$ listedDir <|> Just <$> recursedDir)

    file :: Parser FsItem
    file = File <$> P.decimal <* P.char ' ' <*> word

    listedDir :: Parser ()
    listedDir = () <$ P.string "dir " <* alphaWord

    dirname :: Parser Text
    dirname = P.string "/" <|> alphaWord

solve1 :: FsItem -> Int
solve1 = sum . filter (<= 100000) . dirSizes

solve2 :: FsItem -> Int
solve2 fs =
  let requireFreed = 30000000 - (70000000 - sizeOf fs) in
  head $ filter (>= requireFreed) $ sort $ dirSizes fs

dirSizes :: FsItem -> [Size]
dirSizes (File _ _) = []
dirSizes (Dir size _ fs) = size : foldMap dirSizes fs

mkDir :: Name -> FS -> FsItem
mkDir name fs = Dir (sum $ sizeOf <$> fs) name fs

sizeOf :: FsItem -> Size
sizeOf (File size _) = size
sizeOf (Dir size _ _) = size

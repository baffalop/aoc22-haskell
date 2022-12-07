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
parse = P.parseOnly $ mkDir "/"
  <$ P.string "$ cd /" <* P.endOfLine <* P.string "$ ls" <* P.endOfLine
  <*> fs
  where
    fs :: Parser FS
    fs = catMaybes <$>
      linesOf (Just <$> file <|> Nothing <$ listedDir <|> Just <$> recursedDir)

    file :: Parser FsItem
    file = File <$> P.decimal <* P.char ' ' <*> word

    listedDir :: Parser ()
    listedDir = () <$ P.string "dir " <* alphaWord

    recursedDir :: Parser FsItem
    recursedDir = mkDir
      <$ P.string "$ cd " <*> alphaWord <* P.endOfLine
      <* P.string "$ ls" <* P.endOfLine
      <*> fs <* (P.endOfLine <|> P.endOfInput)
      <* (() <$ P.string "$ cd .." <|> P.endOfInput)

solve1 :: FsItem -> Int
solve1 = sum . filter (<= 100000) . dirSizes

solve2 :: FsItem -> Int
solve2 fs =
  let requireFreed = 30000000 - (70000000 - sizeOf fs) in
  head $ filter (>= requireFreed) $ sort $ dirSizes fs

dirSizes :: FsItem -> [Size]
dirSizes = \case
  (File _ _) -> [0]
  (Dir size _ fs) -> size : concatMap dirSizes fs

mkDir :: Name -> FS -> FsItem
mkDir name fs = Dir (sum $ sizeOf <$> fs) name fs

sizeOf :: FsItem -> Size
sizeOf (File size _) = size
sizeOf (Dir size _ _) = size

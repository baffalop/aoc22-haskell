module Day07 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser)
import Parsing (linesOf, word, alphaWord)
import Control.Applicative ((<|>))
import Data.Maybe (catMaybes)

type FS = [FsItem]

data FsItem
  = File !Size !Name
  | Dir !Size !Name FS
  deriving (Show)

type Size = Int
type Name = Text

parse :: Text -> Either String FS
parse = P.parseOnly $ id
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

solve1 :: FS -> Int
solve1 = sum . filter (<= 100000) . dirSizes

solve2 :: FS -> Int
solve2 = undefined

dirSizes :: FS -> [Size]
dirSizes = concatMap $ \case
  (File _ _) -> [0]
  (Dir size _ fs) -> size : dirSizes fs

mkDir :: Name -> FS -> FsItem
mkDir name fs = Dir (sum $ sizeOf <$> fs) name fs

sizeOf :: FsItem -> Size
sizeOf (File size _) = size
sizeOf (Dir size _ _) = size

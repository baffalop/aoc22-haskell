module Day07 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser)
import Parsing (linesOf, word, alphaWord)
import Control.Applicative ((<|>))
import Data.Maybe (catMaybes)
import Utils ((<.>))
import Control.Monad (void)

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
    recursedDir = do
      name <- P.string "$ cd " <* dirname <* P.endOfLine
      void (P.string "$ ls") <* P.endOfLine
      contents <- fs <* (P.endOfLine <|> P.endOfInput)
      void (P.string "$ cd ..") <|> P.endOfInput
      pure $ Dir (sum $ sizeOf <$> contents) name contents

    fs :: Parser FS
    fs = catMaybes <.> linesOf $ P.choice
      [ Just <.> File <$> P.decimal <* P.char ' ' <*> word
      , Nothing <$ P.string "dir " <* alphaWord
      , Just <$> recursedDir
      ]

    dirname :: Parser Text
    dirname = P.string "/" <|> alphaWord

solve1 :: FsItem -> Int
solve1 = sum . filter (<= 100000) . dirSizes

solve2 :: FsItem -> Int
solve2 fs = minimum $ filter (>= requireFreed) $ dirSizes fs
  where requireFreed = 30000000 - (70000000 - sizeOf fs)

dirSizes :: FsItem -> [Size]
dirSizes (File _ _) = []
dirSizes (Dir size _ fs) = size : foldMap dirSizes fs

sizeOf :: FsItem -> Size
sizeOf (File size _) = size
sizeOf (Dir size _ _) = size

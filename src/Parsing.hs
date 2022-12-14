module Parsing
  (linesOf
  , pairBy
  , word
  , alphaWord
  ) where

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import Data.Text (Text)
import qualified Data.Char as Char

linesOf :: Parser a -> Parser [a]
linesOf = (`P.sepBy` P.endOfLine)

pairBy :: Char -> Parser a -> Parser (a, a)
pairBy sep p = (,) <$> p <* P.char sep <*> p

word :: Parser Text
word = P.takeWhile $ not . Char.isSpace

alphaWord :: Parser Text
alphaWord = P.takeWhile Char.isAlpha

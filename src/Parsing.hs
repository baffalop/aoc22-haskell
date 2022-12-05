module Parsing
  (linesOf
  , pairBy
  ) where

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P

linesOf :: Parser a -> Parser [a]
linesOf = (`P.sepBy` P.endOfLine)

pairBy :: Char -> Parser a -> Parser (a, a)
pairBy sep p = (,) <$> p <* P.char sep <*> p

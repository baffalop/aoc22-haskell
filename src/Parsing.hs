module Parsing
  (linesOf
  , pairBy
  ) where

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import Control.Applicative ((<|>))

linesOf :: Parser a -> Parser [a]
linesOf = (`P.sepBy` (P.endOfLine <|> P.endOfInput))

pairBy :: Char -> Parser a -> Parser (a, a)
pairBy sep p = (,) <$> p <* P.char sep <*> p

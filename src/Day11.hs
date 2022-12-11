module Day11 (parse, solve1, solve2) where

import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Applicative ((<|>))
import Data.Functor (($>))

type Monkeys = Map MonkeyId Monkey

newtype MonkeyId = ID Int deriving (Show, Eq, Ord)
newtype Worry = Worry { worry::Int } deriving (Show, Eq, Ord)

data Monkey = Monkey
  { items :: Seq Worry
  , op :: Worry -> Worry
  , throwTo :: Worry -> MonkeyId
  }

instance Show Monkey where
  show Monkey{..} =
    "[Monkey] Items: " <> show (worry <$> items)
    <> " Worry 2 becomes " <> show (worry $ op $ Worry 2) <> "\n"

parse :: Text -> Either String Monkeys
parse = P.parseOnly $ M.fromList <$> monkey `P.sepBy` P.skipSpace
  where
    monkey :: Parser (MonkeyId, Monkey)
    monkey = do
      monkeyId <- P.string "Monkey " >> ID <$> P.decimal <* P.char ':'
      P.skipSpace <* P.string "Starting items: "
      items <- Seq.fromList <$> (Worry <$> P.decimal) `P.sepBy` P.string ", "
      P.skipSpace
      op <- operation
      P.skipSpace
      throwTo <- test
      pure (monkeyId, Monkey {..})

    operation :: Parser (Worry -> Worry)
    operation = onWorry <$> do
      _ <- P.string "Operation: new = old"
      operator <- (*) <$ P.string " * " <|> (+) <$ P.string " + "
      operator <$> P.decimal <|> P.string "old" $> \x -> x `operator` x

    test :: Parser (Worry -> MonkeyId)
    test = do
      n <- P.string "Test: divisible by " *> P.decimal
      P.skipSpace
      ifTrue <- P.string "If true: throw to monkey " >> ID <$> P.decimal
      P.skipSpace
      ifFalse <- P.string "If false: throw to monkey " >> ID <$> P.decimal
      pure $ \(Worry w) -> if (w `mod` n) == 0 then ifTrue else ifFalse

solve1 :: Monkeys -> Int
solve1 = undefined

solve2 :: Monkeys -> Int
solve2 = undefined

onWorry :: (Int -> Int) -> Worry -> Worry
onWorry f = Worry . f . worry

module Day11 (parse, solve1, solve2) where

import Prelude hiding (round)
import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Monad.State (State, evalState)
import qualified Control.Monad.State as State
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Utils ((<.>))
import Control.Monad (replicateM, forM_)
import Data.Foldable (traverse_, Foldable (toList))
import Data.List (sortOn)

type Monkeys = Map MonkeyId Monkey
type MonkeyState = State Monkeys Monkeys

newtype MonkeyId = ID Int deriving (Show, Eq, Ord)
newtype Worry = Worry { worry::Int } deriving (Show, Eq, Ord)

data Monkey = Monkey
  { items :: Seq Worry
  , amplify :: Worry -> Worry
  , throwTo :: Worry -> MonkeyId
  , inspected :: Int
  }

instance Show Monkey where
  show Monkey{..} =
    "[Monkey] Items: " <> show (worry <$> items)
    <> " Worry 2 becomes " <> show (worry $ amplify $ Worry 2)
    <> " - Inspected: " <> show inspected <> "\n"

parse :: Text -> Either String Monkeys
parse = P.parseOnly $ M.fromList <$> monkey `P.sepBy` P.skipSpace
  where
    monkey :: Parser (MonkeyId, Monkey)
    monkey = do
      monkeyId <- P.string "Monkey " >> ID <$> P.decimal <* P.char ':'
      P.skipSpace <* P.string "Starting items: "
      items <- Seq.fromList <$> (Worry <$> P.decimal) `P.sepBy` P.string ", "
      amplify <- P.skipSpace *> operation
      throwTo <- P.skipSpace *> test
      pure (monkeyId, Monkey { inspected = 0, .. })

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
solve1 = product . take 2 . mostInspected . evalState (doTimes 20 round)
  where
    mostInspected :: Monkeys -> [Int]
    mostInspected = sortOn negate . (inspected <.> toList)

round :: MonkeyState
round = do
  State.gets M.keys >>= traverse_ \k -> do
    monkey@Monkey{..} <- State.gets (! k)
    forM_ items \(reduce . amplify -> worry) ->
      State.modify $ M.adjust (appendItem worry) (throwTo worry)
    State.modify $ M.insert k monkey
      { items = Seq.empty
      , inspected = inspected + length items
      }
  State.get

solve2 :: Monkeys -> Int
solve2 = undefined

appendItem :: Worry -> Monkey -> Monkey
appendItem worry monkey@Monkey{ items } = monkey { items = items Seq.|> worry }

reduce :: Worry -> Worry
reduce = onWorry (`div` 3)

onWorry :: (Int -> Int) -> Worry -> Worry
onWorry f = Worry . f . worry

doTimes :: Int -> State a b -> State a b
doTimes n = last <.> replicateM n

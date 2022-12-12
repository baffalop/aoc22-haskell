{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}

module Day11 (parse, solve1, solve2) where

import Prelude hiding (round)
import Data.Text (Text)
import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Lens.Micro.Platform (makeLensesFor, (%~), (.~))
import Control.Monad.State (State, execState)
import qualified Control.Monad.State as State
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Utils ((<.>))
import Control.Monad (replicateM_, forM_)
import Data.Foldable (traverse_, Foldable (toList))
import Data.List (sortOn)
import Control.Arrow ((>>>))

type Monkeys = Map MonkeyId Monkey
type MonkeyState = State Monkeys ()

newtype MonkeyId = ID Int deriving (Show, Eq, Ord)

type Worry = IWorry Integer
newtype IWorry a = Worry { worry::a } deriving (Show, Eq, Ord, Functor)

data Monkey = Monkey
  { items :: Seq Worry
  , amplify :: Worry -> Worry
  , divisor :: Integer
  , throwTo :: Worry -> MonkeyId
  , inspected :: Int
  }

instance Show Monkey where
  show Monkey{..} =
    "[Monkey] Items: " <> show (worry <$> items)
    <> " Worry 2 becomes " <> show (worry $ amplify $ Worry 2)
    <> " - Inspected: " <> show inspected <> "\n"

makeLensesFor [("items", "_items"), ("inspected", "_inspected")] ''Monkey

parse :: Text -> Either String Monkeys
parse = P.parseOnly $ M.fromList <$> monkey `P.sepBy` P.skipSpace
  where
    monkey :: Parser (MonkeyId, Monkey)
    monkey = do
      monkeyId <- P.string "Monkey " >> ID <$> P.decimal <* P.char ':'
      P.skipSpace <* P.string "Starting items: "
      items <- Seq.fromList <$> (Worry <$> P.decimal) `P.sepBy` P.string ", "
      amplify <- P.skipSpace *> operation
      (divisor, throwTo) <- P.skipSpace *> logic
      pure (monkeyId, Monkey { inspected = 0, .. })

    operation :: Parser (Worry -> Worry)
    operation = fmap <$> do
      _ <- P.string "Operation: new = old"
      operator <- (*) <$ P.string " * " <|> (+) <$ P.string " + "
      operator <$> P.decimal <|> P.string "old" $> \x -> x `operator` x

    logic :: Integral n => Parser (n, IWorry n -> MonkeyId)
    logic = do
      divisor <- P.string "Test: divisible by " *> P.decimal <* P.skipSpace
      ifTrue <- P.string "If true: throw to monkey " >> ID <$> P.decimal <* P.skipSpace
      ifFalse <- P.string "If false: throw to monkey " >> ID <$> P.decimal
      pure $ (divisor,) \(Worry w) -> if (w `mod` divisor) == 0 then ifTrue else ifFalse

solve1 :: Monkeys -> Int
solve1 = scoreMostInspected . execState (replicateM_ 20 $ round $ fmap (`div` 3))

solve2 :: Monkeys -> Int
solve2 monkeys = scoreMostInspected $ execState (replicateM_ 10000 roundModulo) monkeys
  where
    roundModulo :: State Monkeys ()
    roundModulo = do
      round id
      State.modify (fmap $ _items %~ fmap (fmap (`mod` commonDivisor)))

    commonDivisor :: Integer
    commonDivisor = product $ divisor <$> toList monkeys

round :: (Worry -> Worry) -> MonkeyState
round reduce =
  State.gets M.keys >>= traverse_ \k -> do
    Monkey{..} <- State.gets (! k)
    forM_ items $ amplify >>> reduce >>> \worry ->
      State.modify $ M.adjust (_items %~ (|> worry)) (throwTo worry)
    State.modify $ flip M.adjust k $ (_items .~ Seq.empty) . (_inspected %~ (+ length items))

scoreMostInspected :: Monkeys -> Int
scoreMostInspected = product . take 2 . sortOn negate . (inspected <.> toList)

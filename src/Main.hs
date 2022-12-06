module Main (main) where

import qualified Options.Applicative as Opt
import Data.Text (Text)
import qualified Data.Text.IO as T
import Text.Read (readMaybe)
import Data.Either.Extra (maybeToEither)
import Control.Arrow ((&&&))
import Data.Tuple.Extra (both)

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06

type Day = Int

main :: IO ()
main = do
  day <- Opt.execParser cli
  let solve = getSolution day
  input <- T.readFile $ "input/" <> pad0 day <> ".txt"
  case solve input of
    Left err ->
      putStrLn $ "Parse error: " <> err
    Right (sol1, sol2) -> do
      putStrLn $ "Part 1: " <> sol1
      putStrLn $ "Part 2: " <> sol2

getSolution :: Day -> Text -> Either String (String, String)
getSolution 1 = Right . both show . (Day01.solve1 &&& Day01.solve2) . Day01.parse
getSolution 2 = fmap (both show. (Day02.solve1 &&& Day02.solve2)) . Day02.parse
getSolution 3 = Right . both show . (Day03.solve1 &&& Day03.solve2) . Day03.parse
getSolution 4 = fmap (both show . (Day04.solve1 &&& Day04.solve2)) . Day04.parse
getSolution 5 = fmap (Day05.solve1 &&& Day05.solve2) . Day05.parse
getSolution 6 = Right . both show . (Day06.solve1 &&& Day06.solve2) . Day06.parse
getSolution day = error $ "No solution for day " <> show day <> " yet"

cli :: Opt.ParserInfo Day
cli =
  Opt.info (Opt.helper <*> dayArg) $
    Opt.fullDesc
      <> Opt.header "Solutions to Advent of Code 2021"
      <> Opt.progDesc "Run solution(s) for the AoC puzzle of the given day"
  where
    dayArg :: Opt.Parser Day
    dayArg = Opt.argument day $ Opt.metavar "DAY" <> Opt.help "Which day's solution to run"

    day :: Opt.ReadM Day
    day =
      Opt.eitherReader $ \s -> do
        let err = "There are 25 days of Christmas. '" <> s <> "' ain't one of them."
        n <- maybeToEither err $ readMaybe s
        if n < 1 || n > 25
          then Left err
          else Right n

pad0 :: Int -> String
pad0 n = case show n of
  [d] -> ['0', d]
  s -> s

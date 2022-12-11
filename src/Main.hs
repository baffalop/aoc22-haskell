module Main (main) where

import qualified Options.Applicative as Opt
import qualified Advent
import Advent (Day, mkDay, dayInt)
import Data.Text (Text)
import Text.Read (readMaybe)
import Data.Either.Extra (maybeToEither)
import Control.Monad (when)
import Control.Exception (catch, IOException)

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11

newtype SessionKey = Key String
type Solution = Text -> Either String Answer

data Options = Options
  { day :: Day
  , showParsed :: Bool
  }

data Answer = Answer
  { parsed :: String
  , part1 :: String
  , part2 :: String
  }

main :: IO ()
main = do
  Options{ day, showParsed } <- Opt.execParser cli
  key <- Key <$> readFile ".key" `catch` \e ->
    let _ = e :: IOException in
    fail "Ensure you've got the AoC session key in a `.key` file"

  input <- fetchInput day key
  case solve day input of
    Left err ->
      putStrLn $ "Parse error: " <> err
    Right Answer{..} -> do
      when showParsed do
        putStrLn "Parsed input:" >> putStrLn parsed
        putStrLn ""
      putStrLn "Part 1:" >> putStrLn part1
      putStrLn ""
      putStrLn "Part 2:" >> putStrLn part2

solve :: Day -> Solution
solve day = case dayInt day of
  1  -> simpleSolution Day01.parse Day01.solve1 Day01.solve2
  2  -> eitherSolution Day02.parse Day02.solve1 Day02.solve2
  3  -> simpleSolution Day03.parse Day03.solve1 Day03.solve2
  4  -> eitherSolution Day04.parse Day04.solve1 Day04.solve2
  5  -> eitherSolution Day05.parse Day05.solve1 Day05.solve2
  6  -> simpleSolution Day06.parse Day06.solve1 Day06.solve2
  7  -> eitherSolution Day07.parse Day07.solve1 Day07.solve2
  8  -> simpleSolution Day08.parse Day08.solve1 Day08.solve2
  9  -> eitherSolution Day09.parse Day09.solve1 Day09.solve2
  10 -> eitherSolution Day10.parse Day10.solve1 Day10.solve2
  11 -> eitherSolution Day11.parse Day11.solve1 Day11.solve2
  d -> error $ "No solution for day " <> show d <> " yet"

simpleSolution :: (Show a, Show b, Show c) => (Text -> a) -> (a -> b) -> (a -> c) -> Solution
simpleSolution parse = eitherSolution $ Right . parse

eitherSolution :: (Show a, Show b, Show c) => (Text -> Either String a) -> (a -> b) -> (a -> c) -> Solution
eitherSolution parse solve1 solve2 = fmap answer . parse
  where
    answer parsed = Answer
      { parsed = show parsed
      , part1 = show $ solve1 parsed
      , part2 = show $ solve2 parsed
      }

cli :: Opt.ParserInfo Options
cli =
  Opt.info (Opt.helper <*> opts) $ Opt.fullDesc
    <> Opt.header "Solutions to Advent of Code 2021"
    <> Opt.progDesc "Run solution(s) for the AoC puzzle of the given day"
  where
    opts :: Opt.Parser Options
    opts = Options
      <$> Opt.argument readDay (Opt.metavar "DAY" <> Opt.help "Which day's solution to run")
      <*> Opt.switch (Opt.short 's' <> Opt.long "show-parsed" <> Opt.help "Show the parsed input")

    readDay :: Opt.ReadM Day
    readDay = Opt.eitherReader $ \s ->
      let err = "There are 25 days of Christmas. '" <> s <> "' ain't one of them."
      in maybeToEither err $ mkDay =<< readMaybe s

fetchInput :: Day -> SessionKey -> IO Text
fetchInput day key = do
  putStrLn $ "Fetching input for day " <> show (dayInt day) <> "...\n"
  response <- Advent.runAoC (aocOpts key) (Advent.AoCInput day)
  case response of
    Left err -> fail $ "Error response from API: " <> show err
    Right input -> pure input

aocOpts :: SessionKey -> Advent.AoCOpts
aocOpts (Key key) = (Advent.defaultAoCOpts 2022 key) { Advent._aCache = Just "." }

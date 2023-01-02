module Main (main) where

import qualified Options.Applicative as Opt
import qualified Advent
import Advent (Day, mkDay, dayInt)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)
import Data.Either.Extra (maybeToEither)
import Control.Monad (when, forM_)
import Control.Exception (catch, SomeException, IOException)
import Text.Printf (printf)
import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)

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
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day23
import qualified Day25

newtype SessionKey = Key String
type Solution = Text -> Either String Answer

data RunOpts = AllDays | OneDay DayOpts

data DayOpts = DayOpts
  { day :: Day
  , showParsed :: Bool
  , runExample :: Bool
  }

data Answer = Answer
  { parsed :: String
  , part1 :: String
  , part2 :: String
  }

main :: IO ()
main = do
  options <- Opt.execParser cli
  key <- Key <$> readFile ".key" `catch` \e ->
    let _ = e :: IOException in
    fail "Ensure you've got the AoC session key in a `.key` file"

  case options of
    OneDay opts -> runDay key opts
    AllDays -> do
      putStrLn "Running all days...\n"
      forM_ (mapMaybe mkDay [1..25]) \day -> do
        putStrLn $ "----- DAY " <> show (dayInt day)
        runDay key (baseDayOpts day) `catch` \e ->
          let _ = e::SomeException in pure ()
        putStrLn ""

runDay :: SessionKey -> DayOpts -> IO ()
runDay key DayOpts{ day, showParsed, runExample } =
  case solveFor day of
    Nothing -> putStrLn $ "No solution for day " <> show (dayInt day) <> " yet"
    Just solve -> do
      input <- if runExample then readExampleInput day else fetchInput day key

      case solve input of
        Left e -> putStrLn $ "Parse error: " <> e
        Right Answer{..} -> do
          when showParsed do
            putStrLn "Parsed input:" >> putStrLn parsed
            putStrLn ""
          putStrLn "Part 1:" >> putStrLn part1
          putStrLn ""
          putStrLn "Part 2:" >> putStrLn part2

readExampleInput :: Day -> IO Text
readExampleInput (dayInt -> day) =
  TIO.readFile file `catch` \e ->
    let _ = e :: IOException in
    fail $ "Have you created the file " <> file <> " ?"
  where file = "input/2022/ex-" <> printf "%02d" day <> ".txt"

solveFor :: Day -> Maybe Solution
solveFor day = case dayInt day of
  1  -> Just $ simpleSolution Day01.parse Day01.solve1 Day01.solve2
  2  -> Just $ eitherSolution Day02.parse Day02.solve1 Day02.solve2
  3  -> Just $ simpleSolution Day03.parse Day03.solve1 Day03.solve2
  4  -> Just $ eitherSolution Day04.parse Day04.solve1 Day04.solve2
  5  -> Just $ eitherSolution Day05.parse Day05.solve1 Day05.solve2
  6  -> Just $ simpleSolution Day06.parse Day06.solve1 Day06.solve2
  7  -> Just $ eitherSolution Day07.parse Day07.solve1 Day07.solve2
  8  -> Just $ simpleSolution Day08.parse Day08.solve1 Day08.solve2
  9  -> Just $ eitherSolution Day09.parse Day09.solve1 Day09.solve2
  10 -> Just $ eitherSolution Day10.parse Day10.solve1 Day10.solve2
  11 -> Just $ eitherSolution Day11.parse Day11.solve1 Day11.solve2
  12 -> Just $ eitherSolution Day12.parse Day12.solve1 Day12.solve2
  13 -> Just $ eitherSolution Day13.parse Day13.solve1 Day13.solve2
  14 -> Just $ eitherSolution Day14.parse Day14.solve1 Day14.solve2
  15 -> Just $ eitherSolution Day15.parse Day15.solve1 Day15.solve2
  16 -> Just $ eitherSolution Day16.parse Day16.solve1 Day16.solve2
  17 -> Just $ eitherSolution Day17.parse Day17.solve1 Day17.solve2
  18 -> Just $ eitherSolution Day18.parse Day18.solve1 Day18.solve2
  23 -> Just $ simpleSolution Day23.parse Day23.solve1 Day23.solve2
  25 -> Just $ eitherSolution Day25.parse Day25.solve1 Day25.solve2
  _ -> Nothing

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

cli :: Opt.ParserInfo RunOpts
cli =
  Opt.info (Opt.helper <*> allOpt <|> OneDay <$> dayOpts) $ Opt.fullDesc
    <> Opt.header "Solutions to Advent of Code 2021"
    <> Opt.progDesc "Run solution(s) for the AoC puzzle of the given day"
  where
    dayOpts :: Opt.Parser DayOpts
    dayOpts = DayOpts
      <$> Opt.argument readDay (Opt.metavar "DAY" <> Opt.help "Which day's solution to run")
      <*> Opt.switch (Opt.short 's' <> Opt.long "show-parsed" <> Opt.help "Show the parsed input")
      <*> Opt.switch (Opt.short 'e' <> Opt.long "run-example"
        <> Opt.help "Run the solution on the example input at ./input/[year]/ex-[day].txt instead of the problem input. This file needs to be manually created.")

    allOpt :: Opt.Parser RunOpts
    allOpt = Opt.flag' AllDays (Opt.short 'a' <> Opt.long "all"
      <> Opt.help "Run all available days' solutions")

    readDay :: Opt.ReadM Day
    readDay = Opt.eitherReader \s ->
      let err = "There are 25 days of Christmas. '" <> s <> "' ain't one of them."
      in maybeToEither err $ mkDay =<< readMaybe s

fetchInput :: Day -> SessionKey -> IO Text
fetchInput day key = do
  putStrLn $ "Fetching input for day " <> show (dayInt day) <> "...\n"
  response <- Advent.runAoC (aocOpts key) (Advent.AoCInput day)
  case response of
    Left err -> fail $ "Error response from API: " <> show err
    Right input -> pure input

baseDayOpts :: Day -> DayOpts
baseDayOpts day = DayOpts
  { day
  , showParsed = False
  , runExample = False
  }

aocOpts :: SessionKey -> Advent.AoCOpts
aocOpts (Key key) = (Advent.defaultAoCOpts 2022 key) { Advent._aCache = Just "." }

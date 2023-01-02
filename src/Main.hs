{-# LANGUAGE ExistentialQuantification #-}

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
import Control.DeepSeq (NFData)
import qualified Criterion.Main as Criterion
import qualified Criterion as Criterion.Report

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

data Solution = forall p a b . (Show p, Show a, Show b, NFData a, NFData b) =>
  Solution
    { parse :: Text -> Either String p
    , solve1 :: p -> a
    , solve2 :: p -> b
    }

data RunOpts = AllDays | OneDay DayOpts

data DayOpts = DayOpts
  { day :: Day
  , showParsed :: Bool
  , runExample :: Bool
  , benchmark :: Bool
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
runDay key DayOpts{..} = case solutionFor day of
  Nothing ->
    putStrLn $ "No solution for day " <> dayStr <> " yet"

  Just Solution{ parse, solve1, solve2 } -> do
    input <- if runExample then readExampleInput day else fetchInput day key

    when benchmark do
      putStrLn "Parse:"
      Criterion.Report.benchmark $ Criterion.whnf parse input

    case parse input of
      Left e -> putStrLn $ "Parse error: " <> e
      Right parsed -> do
        when showParsed do
          putStrLn "Parsed input:"
          print parsed
          putStrLn ""

        putStrLn "Part 1:"
        when benchmark do
          Criterion.Report.benchmark $ Criterion.nf solve1 parsed
          putStrLn ""
        print $ solve1 parsed

        putStrLn "\nPart 2:"
        when benchmark do
          Criterion.Report.benchmark $ Criterion.nf solve2 parsed
          putStrLn ""
        print $ solve2 parsed
  where
    dayStr = show $ dayInt day

readExampleInput :: Day -> IO Text
readExampleInput (dayInt -> day) =
  TIO.readFile file `catch` \e ->
    let _ = e :: IOException in
    fail $ "Have you created the file " <> file <> " ?"
  where file = "input/2022/ex-" <> printf "%02d" day <> ".txt"

solutionFor :: Day -> Maybe Solution
solutionFor day = case dayInt day of
  1  -> Just $ simpleSn Day01.parse Day01.solve1 Day01.solve2
  2  -> Just $ Solution Day02.parse Day02.solve1 Day02.solve2
  3  -> Just $ simpleSn Day03.parse Day03.solve1 Day03.solve2
  4  -> Just $ Solution Day04.parse Day04.solve1 Day04.solve2
  5  -> Just $ Solution Day05.parse Day05.solve1 Day05.solve2
  6  -> Just $ simpleSn Day06.parse Day06.solve1 Day06.solve2
  7  -> Just $ Solution Day07.parse Day07.solve1 Day07.solve2
  8  -> Just $ simpleSn Day08.parse Day08.solve1 Day08.solve2
  9  -> Just $ Solution Day09.parse Day09.solve1 Day09.solve2
  10 -> Nothing -- Just $ Solution Day10.parse Day10.solve1 Day10.solve2
  11 -> Just $ Solution Day11.parse Day11.solve1 Day11.solve2
  12 -> Just $ Solution Day12.parse Day12.solve1 Day12.solve2
  13 -> Just $ Solution Day13.parse Day13.solve1 Day13.solve2
  14 -> Just $ Solution Day14.parse Day14.solve1 Day14.solve2
  15 -> Just $ Solution Day15.parse Day15.solve1 Day15.solve2
  16 -> Just $ Solution Day16.parse Day16.solve1 Day16.solve2
  17 -> Just $ Solution Day17.parse Day17.solve1 Day17.solve2
  18 -> Just $ Solution Day18.parse Day18.solve1 Day18.solve2
  23 -> Just $ simpleSn Day23.parse Day23.solve1 Day23.solve2
  25 -> Nothing -- Just $ Solution Day25.parse Day25.solve1 Day25.solve2
  _ -> Nothing

simpleSn :: (Show p, Show a, Show b, NFData a, NFData b) => (Text -> p) -> (p -> a) -> (p -> b) -> Solution
simpleSn parse = Solution $ Right . parse

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
      <*> Opt.switch (Opt.short 'b' <> Opt.long "benchmark" <> Opt.help "Benchmark the solutions")

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
baseDayOpts day = DayOpts day False False False

aocOpts :: SessionKey -> Advent.AoCOpts
aocOpts (Key key) = (Advent.defaultAoCOpts 2022 key) { Advent._aCache = Just "." }

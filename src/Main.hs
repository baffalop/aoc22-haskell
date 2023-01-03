{-# LANGUAGE ExistentialQuantification #-}

module Main (main) where

import qualified Options.Applicative as Opt
import qualified Advent
import Advent (Day, mkDay, dayInt)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)
import Data.Either.Extra (maybeToEither)
import Control.Monad (when, unless, forM_)
import Control.Exception (catch, SomeException, IOException)
import Text.Printf (printf)
import Control.Applicative ((<|>))
import Data.Maybe (mapMaybe)
import Data.List ((\\))
import Data.List.Split (splitOn)
import qualified Data.Text.ANSI as ANSI
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

data RunOpts
  = AllDays AllDayOpts
  | OneDay DayOpts

data DayOpts = DayOpts
  { day :: Day
  , showParsed :: Bool
  , runExample :: Bool
  , benchmark :: Bool
  }

data AllDayOpts = AllDayOpts
  { benchmarkAll :: Bool
  , skip :: [Day]
  }

main :: IO ()
main = do
  options <- Opt.execParser cli
  key <- Key <$> readFile ".key" `catch` \e ->
    let _ = e :: IOException in
    fail "Ensure you've got the AoC session key in a `.key` file"

  case options of
    OneDay opts -> runDay key opts
    AllDays AllDayOpts{ benchmarkAll, skip } -> do
      let days = mapMaybe mkDay [1..25] \\ skip
      putStrLn "Running all days...\n"
      forM_ days \day -> do
        printH1 $ "----- DAY " <> show (dayInt day)
        runDay key (baseDayOpts day) { benchmark = benchmarkAll } `catch` \e ->
          putStrLn $ "Error: " <> show (e::SomeException)
        putStrLn ""

runDay :: SessionKey -> DayOpts -> IO ()
runDay key DayOpts{..} = case solutionFor day of
  Nothing ->
    putStrLn $ "No solution for day " <> dayStr <> " yet"

  Just Solution{ parse, solve1, solve2 } -> do
    input <- if runExample then readExampleInput day else fetchInput day key

    when benchmark do
      printH2 "Parse:"
      Criterion.Report.benchmark $ Criterion.whnf parse input
      putStrLn ""

    case parse input of
      Left e -> putStrLn $ "Parse error: " <> e
      Right parsed -> do
        when showParsed do
          unless benchmark $ printH2 "Parse:"
          printValue parsed
          putStrLn ""

        printH2 "Part 1:"
        when benchmark do
          Criterion.Report.benchmark $ Criterion.nf solve1 parsed
          putStrLn ""
        printValue $ solve1 parsed
        putStrLn ""

        printH2 "Part 2:"
        when benchmark do
          Criterion.Report.benchmark $ Criterion.nf solve2 parsed
          putStrLn ""
        printValue $ solve2 parsed

  where
    dayStr = show $ dayInt day

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
  10 -> Just $ Solution Day10.parse Day10.solve1 Day10.solve2
  11 -> Just $ Solution Day11.parse Day11.solve1 Day11.solve2
  12 -> Just $ Solution Day12.parse Day12.solve1 Day12.solve2
  13 -> Just $ Solution Day13.parse Day13.solve1 Day13.solve2
  14 -> Just $ Solution Day14.parse Day14.solve1 Day14.solve2
  15 -> Just $ Solution Day15.parse Day15.solve1 Day15.solve2
  16 -> Just $ Solution Day16.parse Day16.solve1 Day16.solve2
  17 -> Just $ Solution Day17.parse Day17.solve1 Day17.solve2
  18 -> Just $ Solution Day18.parse Day18.solve1 Day18.solve2
  23 -> Just $ simpleSn Day23.parse Day23.solve1 Day23.solve2
  25 -> Just $ Solution Day25.parse Day25.solve1 Day25.solve2
  _ -> Nothing
  where
    simpleSn parse = Solution $ Right . parse

cli :: Opt.ParserInfo RunOpts
cli =
  Opt.info (Opt.helper <*> (OneDay <$> dayOpts) <|> (AllDays <$> allOpt)) $ Opt.fullDesc
    <> Opt.header "Solutions to Advent of Code 2022"
    <> Opt.progDesc "Run solution(s) for the AoC puzzle of the given day"
  where
    dayOpts :: Opt.Parser DayOpts
    dayOpts = DayOpts
      <$> Opt.argument day (Opt.metavar "DAY" <> Opt.help "Which day's solution to run")
      <*> Opt.switch (Opt.short 's' <> Opt.long "show-parsed" <> Opt.help "Show the parsed input")
      <*> Opt.switch (Opt.short 'e' <> Opt.long "example" <> Opt.help "Run the solution on the example input at ./input/[year]/ex-[day].txt instead of the problem input. This file needs to be manually created.")
      <*> benchmarkSwitch

    allOpt :: Opt.Parser AllDayOpts
    allOpt =
      Opt.flag' AllDayOpts (Opt.short 'a' <> Opt.long "all" <> Opt.help "Run all available days' solutions")
      <*> benchmarkSwitch
      <*> Opt.option days (Opt.short 's' <> Opt.long "skip" <> Opt.metavar "DAYS" <> Opt.help "Comma-separated list of days to skip")

    benchmarkSwitch :: Opt.Parser Bool
    benchmarkSwitch = Opt.switch (Opt.short 'b' <> Opt.long "benchmark" <> Opt.help "Benchmark the solutions")

    day :: Opt.ReadM Day
    day = Opt.eitherReader readDay

    days :: Opt.ReadM [Day]
    days = Opt.eitherReader $ traverse readDay . splitOn ","

    readDay :: String -> Either String Day
    readDay s = maybeToEither err $ mkDay =<< readMaybe s
      where err = "There are 25 days of Christmas. '" <> s <> "' ain't one of them."

fetchInput :: Day -> SessionKey -> IO Text
fetchInput day key = do
  putStrLn $ "Fetching input for day " <> show (dayInt day) <> "...\n"
  response <- Advent.runAoC (aocOpts key) (Advent.AoCInput day)
  case response of
    Left err -> fail $ "Error response from API: " <> show err
    Right input -> pure input

readExampleInput :: Day -> IO Text
readExampleInput (dayInt -> day) =
  TIO.readFile file `catch` \e ->
    let _ = e :: IOException in
    fail $ "Have you created the file " <> file <> " ?"
  where file = "input/2022/ex-" <> printf "%02d" day <> ".txt"

printH1 :: String -> IO ()
printH1 = TIO.putStrLn . ANSI.bold . ANSI.magenta . pack

printH2 :: String -> IO ()
printH2 = TIO.putStrLn . ANSI.bold . ANSI.blue . pack

printValue :: Show a => a -> IO ()
printValue = TIO.putStrLn . ANSI.green . pack . show

baseDayOpts :: Day -> DayOpts
baseDayOpts day = DayOpts day False False False

aocOpts :: SessionKey -> Advent.AoCOpts
aocOpts (Key key) = (Advent.defaultAoCOpts 2022 key) { Advent._aCache = Just "." }

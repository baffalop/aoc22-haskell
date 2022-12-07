module Main (main) where

import qualified Options.Applicative as Opt
import qualified Advent
import Advent (Day, mkDay, dayInt)
import Data.Text (Text)
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
import Control.Exception (catch, IOException)

type Solution = Text -> Either String (String, String)
newtype SessionKey = Key String

main :: IO ()
main = do
  day <- Opt.execParser cli
  key <- Key <$> readFile ".key" `catch` \e ->
    let _ = e :: IOException in
    fail "Ensure you've got the AoC session key in a `.key` file"

  input <- fetchInput day key
  case solve day input of
    Left err ->
      putStrLn $ "Parse error: " <> err
    Right (sol1, sol2) -> do
      putStrLn $ "Part 1: " <> sol1
      putStrLn $ "Part 2: " <> sol2

solve :: Day -> Solution
solve day = case dayInt day of
  1 -> simpleSolution Day01.parse Day01.solve1 Day01.solve2
  2 -> eitherSolution Day02.parse Day02.solve1 Day02.solve2
  3 -> simpleSolution Day03.parse Day03.solve1 Day03.solve2
  4 -> eitherSolution Day04.parse Day04.solve1 Day04.solve2
  5 -> eitherSolution Day05.parse Day05.solve1 Day05.solve2
  6 -> simpleSolution Day06.parse Day06.solve1 Day06.solve2
  d -> error $ "No solution for day " <> show d <> " yet"

simpleSolution :: Show b => (Text -> a) -> (a -> b) -> (a -> b) -> Solution
simpleSolution parse = eitherSolution $ Right . parse

eitherSolution :: Show b => (Text -> Either String a) -> (a -> b) -> (a -> b) -> Solution
eitherSolution parse solve1 solve2 = fmap (both show . (solve1 &&& solve2)) . parse

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
    day = Opt.eitherReader $ \s ->
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

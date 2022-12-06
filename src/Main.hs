module Main (main) where

import System.Environment (getArgs)
import Data.Text (Text)
import qualified Data.Text.IO as T
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
  args <- getArgs
  case args of
    [] -> error "Please pass a day number"
    dayString : _ -> do
      let day = read dayString::Day
      let solve = getSolution day
      input <- T.readFile $ "input/" <> pad0 dayString <> ".txt"
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

pad0 :: String -> String
pad0 [n] = ['0', n]
pad0 ns = ns

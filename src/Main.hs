module Main (main) where

import System.Environment (getArgs)
import Control.Arrow ((&&&))

import qualified Day01
import qualified Day02

type Day = Int

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "Please pass a day number"
    dayString : _ -> do
      let day = (read dayString)::Day
      let solve = getSolution day
      input <- readFile $ pad0 dayString ++ ".txt"
      let (sol1, sol2) = solve input
      putStrLn $ "Part 1: " <> show sol1
      putStrLn $ "Part 2: " <> show sol2


getSolution :: Day -> (String -> (Int, Int))
getSolution 1 = (Day01.solve1 &&& Day01.solve2) . Day01.parse
getSolution 2 = (Day02.solve1 &&& Day02.solve2) . Day02.parse
getSolution day = error $ "No solution for day " <> show day <> " yet"


pad0 :: String -> String
pad0 [n] = ['0', n]
pad0 ns = ns

module Main where

import qualified AoC.Day01
import qualified AoC.Day02
import qualified AoC.Day03
import qualified AoC.Day04
import qualified AoC.Day05
import qualified AoC.Day06
import qualified AoC.Day07
import qualified AoC2024.Day01
import qualified AoC2024.Day02
import System.Environment (getArgs)
import qualified Utils.Utils as Utils

parseDay :: String -> String -> String
parseDay suffix ('2' : '4' : a : b : _) = "Y2024_" ++ parseDay suffix [a, b]
parseDay suffix (a : b : _) = "Day" ++ [a, b] ++ suffix ++ ".txt"
parseDay _ _ = error "Invalid day format."

parseArgs :: String -> (String, String)
parseArgs ('e' : day) = (day, "input/" ++ parseDay "_Example" day)
parseArgs day = (day, "input/" ++ parseDay "" day)

getDaySolution :: String -> Utils.Solution
getDaySolution "2401" = AoC2024.Day01.solution
getDaySolution "2402" = AoC2024.Day02.solution
getDaySolution "01" = AoC.Day01.solution
getDaySolution "02" = AoC.Day02.solution
getDaySolution "03" = AoC.Day03.solutionOpt
getDaySolution "04" = AoC.Day04.solution
getDaySolution "05" = AoC.Day05.solution
getDaySolution "06" = AoC.Day06.solution
getDaySolution "07" = AoC.Day07.solution
getDaySolution _ = error "No solution-implementation found for the given day."

printSolution :: String -> [String] -> IO ()
printSolution day inputLines = do
  let (part1, part2) = getDaySolution day
  putStrLn $ "Part 1: " ++ show (part1 inputLines)
  putStrLn $ "Part 2: " ++ show (part2 inputLines)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "Please provide the day as an argument."
    ["debug"] -> do
      print "Debug mode not implemented."
    _ -> do
      let (day, inputFile) = parseArgs (head args)
      contents <- readFile inputFile
      let inputLines = lines contents
      printSolution day inputLines

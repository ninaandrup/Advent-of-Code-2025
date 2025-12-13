module AoC.Day10 (solution) where

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import qualified Data.Sequence as Seq
import Debug.Trace (trace)
import qualified Utils.Utils as Utils

data ParsedLine = ParsedLine
  { indicatorLights :: [Bool],
    buttonWires :: [[Bool]],
    wireValues :: [Int]
  }
  deriving (Show)

parsing :: [String] -> [ParsedLine]
parsing = map parseLine

parseLine :: String -> ParsedLine
parseLine line =
  let parts = words line
      indicators = parseIndicatorLights (head parts)
      len = length indicators
      middleParts = takeWhile (\s -> head s == '(') (tail parts)
      buttons = map (parseButtonWires len) middleParts
      lastPart = last parts
      values = parseWireValues lastPart
   in ParsedLine {indicatorLights = indicators, buttonWires = buttons, wireValues = values}

-- Parse [.##.] to [False, True, True, False]
parseIndicatorLights :: String -> [Bool]
parseIndicatorLights s =
  let content = filter (\c -> c == '.' || c == '#') s
   in map (== '#') content

-- Parse (3) or (1,3) to [False, False, False, True] or [False, True, False, True]
parseButtonWires :: Int -> String -> [Bool]
parseButtonWires len s =
  let content = filter (\c -> isDigit c || c == ',') s
      indices = map read (splitOn "," content) :: [Int]
   in [i `elem` indices | i <- [0 .. len - 1]]

-- Parse {3,5,4,7} to [3,5,4,7]
parseWireValues :: String -> [Int]
parseWireValues s =
  let content = filter (\c -> isDigit c || c == ',') s
   in map read (splitOn "," content)

part1 :: Utils.SolutionSingle
part1 input =
  let parsedInput = parsing input
   in trace (show parsedInput) 0

part2 :: Utils.SolutionSingle
part2 _ = 0

solution :: Utils.Solution
solution = (part1, part2)

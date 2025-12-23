module AoC.Day10 (solution) where

import Data.Char (isDigit)
import Data.List (transpose)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.Split (splitOn)
import qualified Data.Sequence as Seq
import Debug.Trace (trace)
import qualified Utils.Utils as Utils

-- PARSING --

type IndicatorLight = [Bool]

type ButtonWire = [Bool]

type JoltageRequirement = [Int]

data ParsedLine = ParsedLine
  { indicatorLights :: IndicatorLight,
    buttonWires :: Seq.Seq ButtonWire,
    numButtonWires :: Int,
    joltageRequirement :: JoltageRequirement
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
      requirements = parseJoltageRequirement lastPart
   in ParsedLine
        { indicatorLights = indicators,
          buttonWires = Seq.fromList buttons,
          numButtonWires = length buttons,
          joltageRequirement = requirements
        }

-- Parse [.##.] to [False, True, True, False]
parseIndicatorLights :: String -> IndicatorLight
parseIndicatorLights s =
  let content = filter (\c -> c == '.' || c == '#') s
   in map (== '#') content

-- Parse (3) or (1,3) to [False, False, False, True] or [False, True, False, True]
parseButtonWires :: Int -> String -> ButtonWire
parseButtonWires len s =
  let content = filter (\c -> isDigit c || c == ',') s
      indices = map read (splitOn "," content) :: [Int]
   in [i `elem` indices | i <- [0 .. len - 1]]

-- Parse {3,5,4,7} to [3,5,4,7]
parseJoltageRequirement :: String -> JoltageRequirement
parseJoltageRequirement s =
  let content = filter (\c -> isDigit c || c == ',') s
   in map read (splitOn "," content)

-- Generate all combinations of k elements from length n
combinations :: Int -> Int -> [[Int]]
combinations n = go 0
  where
    go :: Int -> Int -> [[Int]]
    go i k
      | k < 0 = error "k must be non-negative"
      | n < k = error "k must not be greater than n"
      | k == 0 = [[]]
      | k == n - i = [[i .. n - 1]]
      | otherwise = map (i :) (go (i + 1) (k - 1)) ++ go (i + 1) k

-- SOLUTION --

pressButtonsResult :: [ButtonWire] -> IndicatorLight
pressButtonsResult = map (NonEmpty.xor . NonEmpty.fromList) . transpose

getButtons :: [Int] -> Seq.Seq ButtonWire -> [ButtonWire]
getButtons indices wires = map (Seq.index wires) indices

fewestButtonPresses :: ParsedLine -> Int
fewestButtonPresses parsedLine = go 1
  where
    go :: Int -> Int
    go presses = if result then presses else go (presses + 1)
      where
        pressCombinations = combinations (numButtonWires parsedLine) presses
        buttonCombinations = map (`getButtons` buttonWires parsedLine) pressCombinations
        result = foldl (\acc press -> acc || pressButtonsResult press == indicatorLights parsedLine) False buttonCombinations

part1 :: Utils.SolutionSingle
part1 input =
  let parsedInput = parsing input
   in sum $ map fewestButtonPresses parsedInput

part2 :: Utils.SolutionSingle
part2 _ = 0

solution :: Utils.Solution
solution = (part1, part2)

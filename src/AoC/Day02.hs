module AoC.Day02 where

import Data.List.Split (splitOn)
import qualified Utils

type Range = (Int, Int)

parsing :: [String] -> [Range]
parsing input =
  Utils.splitInputToPair read "-" . splitOn "," $ head input

isTwiceRepeat :: String -> Bool
isTwiceRepeat numStr
  | even (length numStr) =
      let (left, right) = splitAt (length numStr `div` 2) numStr
       in left == right
  | otherwise = False

isPrefixRepeat :: String -> String -> Bool
isPrefixRepeat prefix numStr
  | null numStr = True
  | length numStr `mod` prefixLen == 0 =
      let currPrefix = take prefixLen numStr
          numStrNoPrefix = drop prefixLen numStr
       in ((currPrefix == prefix) && isPrefixRepeat prefix numStrNoPrefix)
  | otherwise = False
  where
    prefixLen = length prefix

isMultiRepeat :: String -> Bool
isMultiRepeat numStr =
  any (\len -> isPrefixRepeat (take len numStr) numStr) [1 .. (length numStr `div` 2)]

sumInvalidIDs :: (String -> Bool) -> Range -> Int
sumInvalidIDs isInvalidID (start, end)
  | start > end = 0
  | otherwise =
      if isInvalidID (show start)
        then start + sumInvalidIDs isInvalidID (start + 1, end)
        else sumInvalidIDs isInvalidID (start + 1, end)

solution1 :: Utils.SolutionSingle
solution1 input =
  let ranges = parsing input
   in sum (map (sumInvalidIDs isTwiceRepeat) ranges)

solution2 :: Utils.SolutionSingle
solution2 input =
  let ranges = parsing input
   in sum (map (sumInvalidIDs isMultiRepeat) ranges)

solution :: Utils.Solution
solution = (solution1, solution2)

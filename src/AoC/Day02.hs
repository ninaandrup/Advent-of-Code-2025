module AoC.Day02 where

import Data.List.Split (chunksOf, splitOn)
import qualified Utils.Utils as Utils

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

isMultiRepeatHelper :: [String] -> Bool
isMultiRepeatHelper xs = all (== head xs) xs

isMultiRepeat :: String -> Bool
isMultiRepeat numStr =
  any (\len -> isMultiRepeatHelper $ chunksOf len numStr) [1 .. (length numStr `div` 2)]

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

module Utils.Utils where

import Data.List.Split (splitOn)

type SolutionSingle = [String] -> Int

type Solution = (SolutionSingle, SolutionSingle)

splitInputToPairHelper :: (String -> a) -> String -> String -> (a, a)
splitInputToPairHelper converter substr input =
  case splitOn substr input of
    [a, b] -> (converter a, converter b)
    _ -> error "Substring not found in input."

splitInputToPair :: (String -> a) -> String -> [String] -> [(a, a)]
splitInputToPair converter substr = map (splitInputToPairHelper converter substr)

splitInputToList :: (String -> a) -> String -> [String] -> [[a]]
splitInputToList converter substr = map (map converter . splitOn substr)

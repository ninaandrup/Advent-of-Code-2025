module AoC2024.Day01 (solution) where

import qualified Data.Bifunctor
import Data.List (sort)
import qualified Utils.Utils as Utils

parsing :: [String] -> ([Int], [Int])
parsing = unzip . Utils.splitInputToPair (\x -> read x :: Int) "   "

distance :: (Int, Int) -> Int
distance (x, y) = abs (x - y)

solution1 :: Utils.SolutionSingle
solution1 inputLines =
  let (left, right) = Data.Bifunctor.bimap sort sort $ parsing inputLines
   in sum $ zipWith (curry distance) left right

countOccurrences :: Int -> [Int] -> Int
countOccurrences x = length . filter (== x)

solution2 :: Utils.SolutionSingle
solution2 inputLines =
  let (left, right) = parsing inputLines
      counts = map (\x -> (x, countOccurrences x right)) left
   in sum $ map (uncurry (*)) counts

solution :: Utils.Solution
solution = (solution1, solution2)

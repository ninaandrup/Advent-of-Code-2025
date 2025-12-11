{-# LANGUAGE TupleSections #-}

module AoC.Day09 (solution) where

import Debug.Trace (trace)
import qualified Utils.Utils as Utils

pairs :: [a] -> [(a, a)]
pairs (x : xs) = map (x,) xs ++ pairs xs
pairs [] = []

area :: (Int, Int) -> (Int, Int) -> Int
area (x1, y1) (x2, y2) =
  abs (x1 - x2 + 1) * abs (y1 - y2 + 1)

part1 :: Utils.SolutionSingle
part1 input =
  let points = Utils.splitInputToPair read "," input :: [(Int, Int)]
      pointPairs = pairs points
      areas = map (uncurry area) pointPairs
   in maximum areas

-- By visual inspection of the input,
-- part 2 must include either one of these points:
--  (94870, 50025) or (94870, 48753)

point1 :: (Int, Int)
point1 = (94870, 50025)

point2 :: (Int, Int)
point2 = (94870, 48753)

leftAbovePoint1 :: (Int, Int) -> Bool
leftAbovePoint1 (x, y) = x < fst point1 && snd point1 < y

leftBelowPoint2 :: (Int, Int) -> Bool
leftBelowPoint2 (x, y) = x < fst point2 && y < snd point2

partitionPoints :: [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])
partitionPoints [] = ([], [])
partitionPoints (p : ps)
  | leftAbovePoint1 p = let (xs, ys) = partitionPoints ps in (p : xs, ys)
  | leftBelowPoint2 p = let (xs, ys) = partitionPoints ps in (xs, p : ys)
  | otherwise = partitionPoints ps

part2 :: Utils.SolutionSingle
part2 input =
  let points = Utils.splitInputToPair read "," input :: [(Int, Int)]
      (subGroup1, subGroup2) = partitionPoints points
      subGroup1Areas = map (area point1) $ trace (show subGroup1) subGroup1
      subGroup2Areas = map (area point2) subGroup2
   in maximum subGroup1Areas `max` maximum subGroup2Areas

solution :: Utils.Solution
solution = (part1, part2)

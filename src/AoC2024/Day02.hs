module AoC2024.Day02 (solution) where

import qualified Utils

parsing :: [String] -> [[Int]]
parsing = Utils.splitInputToList (\x -> read x :: Int) " "

decreasing :: (Int, Int) -> Int
decreasing (x, y) = x - y

increasing :: (Int, Int) -> Int
increasing (x, y) = y - x

safe :: [Int] -> Bool
safe [] = True
safe [_] = True
safe (x1 : x2 : xs) =
  let f = if x1 < x2 then increasing else decreasing
      accf (prev, acc) curr =
        let difference = f (prev, curr)
         in (curr, (1 <= difference) && (difference <= 3) && acc)
   in snd $ foldl accf (x1, True) (x2 : xs)

solution1 :: Utils.SolutionSingle
solution1 = length . filter id . map safe . parsing

solution :: Utils.Solution
solution = (solution1, const 0)

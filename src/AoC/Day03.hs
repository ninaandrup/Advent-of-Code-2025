module AoC.Day03 where

import Data.List (elemIndex)
import qualified Data.Map as Map
import qualified Utils

type JoltageMap = Map.Map Int Int

parsing :: [String] -> [[Int]]
parsing = map (map (\c -> read [c] :: Int))

insertJoltageMapValue :: Int -> JoltageMap -> JoltageMap
insertJoltageMapValue joltage =
  Map.map (max joltage)

insertJoltageMapKey :: Int -> JoltageMap -> JoltageMap
insertJoltageMapKey joltage joltageMap
  | Map.notMember joltage joltageMap = Map.insert joltage (-1) joltageMap
  | otherwise = joltageMap

insertJoltageMap :: Int -> JoltageMap -> JoltageMap
insertJoltageMap joltage = insertJoltageMapKey joltage . insertJoltageMapValue joltage

constructJoltageMap :: [Int] -> JoltageMap
constructJoltageMap = foldl (flip insertJoltageMap) Map.empty

findLargestJoltage :: JoltageMap -> Int
findLargestJoltage = maximum . Map.mapWithKey (\k v -> if v == -1 then 0 else 10 * k + v)

solution1 :: Utils.SolutionSingle
solution1 input =
  let joltageRatings = parsing input
      joltageMappings = map constructJoltageMap joltageRatings
      largestJoltages = map findLargestJoltage joltageMappings
   in sum largestJoltages

findNthDigit :: Int -> [Int] -> (Int, [Int])
findNthDigit n digits =
  let subDigits = reverse . drop (12 - n) . reverse $ digits
      largestDigit = maximum subDigits
   in case elemIndex largestDigit digits of
        Just idx -> (largestDigit, drop (idx + 1) digits)
        Nothing -> error "Error: Largest digit not found"

constructLargestJoltage :: [Int] -> [Int]
constructLargestJoltage digits =
  let foldF (subDigits, res) n =
        let (largestDigit, remainingDigits) = findNthDigit n subDigits
         in (remainingDigits, largestDigit : res)
   in reverse . snd $ foldl foldF (digits, []) [1 .. 12]

digitsToInt :: [Int] -> Int
digitsToInt = read . concatMap show

solution2 :: Utils.SolutionSingle
solution2 input =
  let joltageRatings = parsing input
      largestJoltages = map constructLargestJoltage joltageRatings
   in sum $ map digitsToInt largestJoltages

solution :: Utils.Solution
solution = (solution1, solution2)

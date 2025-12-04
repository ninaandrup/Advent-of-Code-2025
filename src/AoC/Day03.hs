module AoC.Day03 (solution, solutionOpt) where

import Data.List (elemIndex)
import qualified Data.Map as Map
import qualified Utils.Utils as Utils

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

largestJoltageRatingAux :: (Int, Maybe Int) -> [Int] -> (Int, Int)
largestJoltageRatingAux _ [] = error "Not enough digits provided"
largestJoltageRatingAux (v1, v2) [x] =
  case v2 of
    Just v2' -> (v1, max v2' x)
    Nothing -> (v1, x)
largestJoltageRatingAux (v1, v2) (x : xs)
  | v1 < x = largestJoltageRatingAux (x, Nothing) xs
  | otherwise = case v2 of
      Just v2' -> largestJoltageRatingAux (v1, Just (max v2' x)) xs
      Nothing -> largestJoltageRatingAux (v1, Just x) xs

largestJoltageRating :: [Int] -> Int
largestJoltageRating [] = error "No digits provided"
largestJoltageRating (x : xs) =
  let (v1, v2) = largestJoltageRatingAux (x, Nothing) xs
   in v1 * 10 + v2

solution1Opt :: Utils.SolutionSingle
solution1Opt input = sum . map largestJoltageRating $ parsing input

getLastTwelve :: [Int] -> [Int]
getLastTwelve digits = drop (length digits - 12) digits

compareLastTwelve :: [(Int, Maybe Int)] -> [Int]
compareLastTwelve [] = []
compareLastTwelve ((x1, Nothing) : xs) = x1 : map fst xs
compareLastTwelve ((x1, Just x2) : xs)
  | x2 < x1 = x1 : map fst xs
  | otherwise = x2 : compareLastTwelve xs

addNumber :: Int -> [Maybe Int] -> [Maybe Int]
addNumber _ [] = []
addNumber x (Nothing : ys) = Just x : ys
addNumber x (Just y : ys)
  | y < x = Just x : map (const Nothing) ys
  | otherwise = Just y : addNumber x ys

largestJoltageRatingAuxV2 :: Int -> [Maybe Int] -> [Int] -> [Maybe Int]
largestJoltageRatingAuxV2 _ possibleRatings [] = possibleRatings
largestJoltageRatingAuxV2 i possibleRatings (x : xs)
  | i < 12 =
      let possibleRatings' = take (12 - i) possibleRatings ++ addNumber x (drop (12 - i) possibleRatings)
       in largestJoltageRatingAuxV2 (i - 1) possibleRatings' xs
  | otherwise = largestJoltageRatingAuxV2 (i - 1) (addNumber x possibleRatings) xs

largestJoltageRatingV2 :: [Int] -> Int
largestJoltageRatingV2 ratings =
  let possibleSolution = largestJoltageRatingAuxV2 (length ratings) (replicate 12 Nothing) ratings
      lastTwelveRatings = getLastTwelve ratings
      finalRatings = compareLastTwelve (zip lastTwelveRatings possibleSolution)
   in read . concatMap show $ finalRatings

solution2Opt :: Utils.SolutionSingle
solution2Opt input = sum . map largestJoltageRatingV2 $ parsing input

solutionOpt :: Utils.Solution
solutionOpt = (solution1Opt, solution2Opt)

module AoC.Day05 (solution) where

import Data.List (sort)
import Data.List.Split (splitWhen)
import qualified Utils.BBST as BBST
import qualified Utils.Utils as Utils

newtype Range = Range (Int, Int)

instance Eq Range where
  (Range (x1, x2)) == (Range (y1, y2)) = x1 == y1 && x2 == y2

instance Ord Range where
  compare (Range (x1, _)) (Range (y1, _)) = compare x1 y1

instance Show Range where
  show (Range (x1, x2)) = show (x1, x2)

type IngredientID = Int

parsing :: [String] -> ([Range], [IngredientID])
parsing input = case splitWhen (== "") input of
  [rangesStr, idsStr] ->
    let ranges = map Range $ Utils.splitInputToPair read "-" rangesStr
        ids = map read idsStr
     in (ranges, ids)
  _ -> error "Invalid input"

mergeRangesAux :: [Range] -> [Range]
mergeRangesAux [] = []
mergeRangesAux [r] = [r]
mergeRangesAux (Range (x1, x2) : Range (y1, y2) : rs)
  | y1 <= x2 + 1 = mergeRangesAux (Range (x1, max x2 y2) : rs)
  | otherwise = Range (x1, x2) : mergeRangesAux (Range (y1, y2) : rs)

mergeRanges :: [Range] -> [Range]
mergeRanges = mergeRangesAux . sort

predecessor :: Int -> [Range] -> Maybe Range
predecessor i ranges =
  let bbst = BBST.fromList $ map (\r@(Range (x1, _)) -> (x1, r)) ranges
   in BBST.predecessor i bbst >>= Just . snd

ingredientIsFresh :: IngredientID -> [Range] -> Bool
ingredientIsFresh i rs = case predecessor i rs of
  Just (Range (_, x2)) -> i <= x2
  Nothing -> False

solution1 :: Utils.SolutionSingle
solution1 input =
  let (ranges, ids) = parsing input
      mergedRanges = mergeRanges ranges
   in length $ filter (`ingredientIsFresh` mergedRanges) ids

solution2 :: Utils.SolutionSingle
solution2 input = 0 -- Placeholder implementation for Part 2

solution :: Utils.Solution
solution = (solution1, solution2)

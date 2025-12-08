module AoC.Day07 (solution) where

import qualified Data.Bifunctor
import Data.List (elemIndex, elemIndices, transpose)
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Utils.Utils as Utils

parsing :: [String] -> (V.Vector Bool, [V.Vector Bool])
parsing input =
  let startVector = V.fromList . map (== 'S') $ head input
      noEmptyLines = filter (not . all (== '.')) input
      grid = map (V.fromList . map (== '^')) $ noEmptyLines
   in (startVector, grid)

step :: V.Vector Bool -> [V.Vector Bool] -> Int
step _ [] = 0
step currentBeamPos (row : rows) =
  let (splitCounts, allUpdates) =
        V.foldl concatUpdates (0, []) $ V.izipWith (,,) currentBeamPos row
   in splitCounts + step (currentBeamPos V.// allUpdates) rows
  where
    hitSplitter :: Int -> Bool -> Bool -> (Int, [(Int, Bool)])
    hitSplitter idx beam splitter
      | beam && splitter = (1, [(idx - 1, True), (idx, False), (idx + 1, True)])
      | otherwise = (0, [])

    concatUpdates :: (Int, [(Int, Bool)]) -> (Int, Bool, Bool) -> (Int, [(Int, Bool)])
    concatUpdates (splits, updates) (idx, beam, splitter) =
      let (s, u) = hitSplitter idx beam splitter
       in (splits + s, updates ++ u)

part1 :: Utils.SolutionSingle
part1 input =
  let (startVector, grid) = parsing input
   in step startVector grid

data NewGrid
  = NewGrid
  { height :: Int,
    gridRowCol :: Map.Map Int [Int],
    gridColRow :: Map.Map Int [Int]
  }

type SplitterValues = Map.Map (Int, Int) Int

parsingPart2 :: [String] -> NewGrid
parsingPart2 input =
  let nonEmptyInput = filter (not . all (== '.')) input
      inputRows = zip [0 ..] nonEmptyInput
      inputCols = zip [0 ..] $ transpose nonEmptyInput

      findSplitters = map (Data.Bifunctor.second (elemIndices '^'))
   in NewGrid
        { height = length inputCols - 1,
          gridRowCol = Map.fromList $ findSplitters inputRows,
          gridColRow = Map.fromList $ findSplitters inputCols
        }

getSplitValueBelow :: (Int, Int) -> NewGrid -> SplitterValues -> Int
getSplitValueBelow (row, col) grid splitterValues =
  case closestSplitter of
    Nothing -> 1
    Just r -> splitterValues ! (r, col)
  where
    sameCol = gridColRow grid ! col
    closestSplitter = Set.lookupGT row $ Set.fromList sameCol

computeSplitterValues :: Int -> NewGrid -> SplitterValues -> SplitterValues
computeSplitterValues 0 _ splitterValues = splitterValues
computeSplitterValues row grid splitterValues =
  case Map.lookup row $ gridRowCol grid of
    Nothing -> next splitterValues
    Just [] -> next splitterValues
    Just cols -> next newSplitterValues
      where
        splitValuesBelow c = getSplitValueBelow (row, c) grid splitterValues
        colSplitterValues = map (\c -> ((row, c), splitValuesBelow (c - 1) + splitValuesBelow (c + 1))) cols
        newSplitterValues = splitterValues `Map.union` Map.fromList colSplitterValues
  where
    next = computeSplitterValues (row - 1) grid

part2 :: Utils.SolutionSingle
part2 input =
  let startPos = (0, Maybe.fromJust . elemIndex 'S' $ head input)
      splittersGrid = parsingPart2 input
      splitterValues = computeSplitterValues (height splittersGrid) splittersGrid Map.empty
   in getSplitValueBelow startPos splittersGrid splitterValues

solution :: Utils.Solution
solution = (part1, part2)

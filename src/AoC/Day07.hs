{-# LANGUAGE TupleSections #-}

module AoC.Day07 (solution) where

import qualified Data.Bifunctor
import Data.List (elemIndex, elemIndices, transpose)
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Vector as V
import Debug.Trace (trace)
import qualified Utils.Grid as G
import qualified Utils.Utils as Utils

parsing :: [String] -> ((Int, Int), G.Grid Bool)
parsing input =
  let startPos = (0, Maybe.fromJust . elemIndex 'S' $ head input)
      noEmptyLines = filter (not . all (== '.')) input
      grid = G.fromGridList . map (map (== '^')) $ noEmptyLines
   in (startPos, grid)

adjacent :: G.Grid Bool -> G.Adjacent
adjacent grid (x, y) =
  let possibleAdj = case G.getCellSafe below grid of
        Just True -> [(x + 1, y - 1), (x + 1, y + 1)]
        Just False -> [(x + 1, y)]
        Nothing -> []
   in filter (`G.validCell` grid) possibleAdj
  where
    below = (x + 1, y)

part1 :: Utils.SolutionSingle
part1 input =
  let (startPos, grid) = parsing input
      dfsVisited = G.dfs startPos (adjacent grid)
      hitSplitter (x, y) = Maybe.fromMaybe False $ G.getCellSafe (x + 1, y) grid
   in length . filter hitSplitter $ Set.toList dfsVisited

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

module AoC.Day04 (solution) where

import Data.Bifunctor (second)
import qualified Data.Vector as V
import qualified Utils.Grid as Grid
import qualified Utils.Utils as Utils

getValidAdjacentIndices :: Int -> Grid.Grid a -> V.Vector Int
getValidAdjacentIndices idx grid =
  let isLeftEdge = idx `mod` Grid.width grid == 0
      isRightEdge = (idx + 1) `mod` Grid.width grid == 0

      left = if isLeftEdge then [] else [above - 1, idx - 1, below - 1]
      right = if isRightEdge then [] else [above + 1, idx + 1, below + 1]

      adjacentIndices = [above, below] ++ left ++ right
   in V.filter (\i -> 0 <= i && i < len) (V.fromList adjacentIndices)
  where
    above = idx - Grid.width grid
    below = idx + Grid.width grid
    len = V.length (Grid.cells grid)

getAdjacent :: Int -> Grid.Grid a -> V.Vector a
getAdjacent idx grid = V.map (\i -> Grid.cells grid V.! i) (getValidAdjacentIndices idx grid)

adjacentCount :: (a -> Bool) -> Grid.Grid a -> Grid.Grid (a, Int)
adjacentCount predicate grid =
  let adjacentElems = V.imap (\i v -> (v, getAdjacent i grid)) $ Grid.cells grid
      adjacentCountVec = V.map (second (length . V.filter id . V.map predicate)) adjacentElems
   in Grid.Grid {Grid.width = Grid.width grid, Grid.height = Grid.height grid, Grid.cells = adjacentCountVec}

countGrid :: (a -> Bool) -> Grid.Grid a -> Int
countGrid predicate = V.length . V.filter predicate . Grid.cells

parsing :: [String] -> Grid.Grid Bool
parsing input =
  let gridIsRoll = map (map (== '@')) input
   in Grid.fromGridList gridIsRoll

solution1 :: Utils.SolutionSingle
solution1 input =
  let grid = parsing input
      adjacentRollCount = adjacentCount id grid
   in countGrid (\(v, c) -> v && c < 4) adjacentRollCount

solution2Aux :: Grid.Grid Bool -> Int
solution2Aux grid =
  let adjacentRollCount = adjacentCount id grid
      countRemoved = countGrid (\(v, c) -> v && c < 4) adjacentRollCount
      newGrid = fmap (\(v, c) -> not (v && c < 4) && v) adjacentRollCount
   in countRemoved + if countRemoved == 0 then 0 else solution2Aux newGrid

solution2 :: Utils.SolutionSingle
solution2 input =
  let grid = parsing input
   in solution2Aux grid

solution :: Utils.Solution
solution = (solution1, solution2)

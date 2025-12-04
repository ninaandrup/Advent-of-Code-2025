module AoC.Day04 where

import qualified Utils.Grid as Grid
import qualified Utils.Utils as Utils

parsing :: [String] -> Grid.Grid Bool
parsing input =
  let gridIsRoll = map (map (== '@')) input
   in Grid.fromListGrid gridIsRoll

rollPositions :: Grid.Grid Bool -> [(Int, Int)]
rollPositions grid =
  let allPos = [(r, c) | r <- [0 .. Grid.height grid - 1], c <- [0 .. Grid.width grid - 1]]
   in filter (\(r, c) -> Grid.lookupGrid r c grid == Just True) allPos

solution1 :: Utils.SolutionSingle
solution1 input =
  let grid = parsing input
      adjacentRollCount = Grid.adjecentCount id grid
   in Grid.countGrid (\(v, c) -> v && c < 4) adjacentRollCount

solution2Aux :: Grid.Grid Bool -> Int
solution2Aux grid =
  let adjacentRollCount = Grid.adjecentCount id grid
      countRemoved = Grid.countGrid (\(v, c) -> v && c < 4) adjacentRollCount
      newGrid = Grid.mapGrid (\(v, c) -> not (v && c < 4) && v) adjacentRollCount
   in countRemoved + if countRemoved == 0 then 0 else solution2Aux newGrid

solution2 :: Utils.SolutionSingle
solution2 input =
  let grid = parsing input
   in solution2Aux grid

solution :: Utils.Solution
solution = (solution1, solution2)

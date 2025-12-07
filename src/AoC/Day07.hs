module AoC.Day07 (solution) where

import Data.List (elemIndex)
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Debug.Trace (trace)
import qualified Utils.Grid as Grid
import qualified Utils.Utils as Utils

part1 :: Utils.SolutionSingle
part1 input =
  let startPos = Maybe.fromJust . elemIndex 'S' $ head input
      grid = Grid.fromGridList . map (map (== '^')) $ tail input
      dfsVisited = Grid.dfs (0, startPos) (adjacent grid)
      hitSplitter (x, y) = Maybe.fromMaybe False $ Grid.getCellSafe (x + 1, y) grid
   in length . filter hitSplitter $ Set.toList dfsVisited
  where
    adjacent :: Grid.Grid Bool -> Grid.Adjacent
    adjacent grid (x, y) =
      let below = (x + 1, y)
          possibleAdj = case Grid.getCellSafe below grid of
            Just True -> [(x + 1, y - 1), (x + 1, y + 1)]
            Just False -> [(x + 1, y)]
            Nothing -> []
       in filter (`Grid.validCell` grid) possibleAdj

part2 :: Utils.SolutionSingle
part2 _ = 0

solution :: Utils.Solution
solution = (part1, part2)

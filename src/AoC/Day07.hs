module AoC.Day07 (solution) where

import Data.List (elemIndex)
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

modifiedDFS :: G.CellPos -> G.Grid Bool -> Int
modifiedDFS start tachyonManifold = fst $ modifiedDFSIter start resStartGrid
  where
    resStartGrid :: G.Grid (Maybe Int)
    resStartGrid =
      G.Grid
        { G.width = G.width tachyonManifold,
          G.height = G.height tachyonManifold,
          G.cells = V.map (const Nothing) $ G.cells tachyonManifold
        }
    modifiedDFSIter :: G.CellPos -> G.Grid (Maybe Int) -> (Int, G.Grid (Maybe Int))
    modifiedDFSIter cellPos timelineGrid =
      case G.getCell cellPos timelineGrid of
        Just v -> (v, timelineGrid)
        Nothing ->
          let adjacentPosistions = adjacent tachyonManifold cellPos
           in case adjacentPosistions of
                [] -> (1, G.setCell cellPos (Just 1) timelineGrid)
                _ ->
                  foldl
                    ( \(sumAcc, timelineGridAcc) adjPos ->
                        let (num, timelineGridAcc') = modifiedDFSIter adjPos timelineGridAcc
                         in (trace (show num) $ sumAcc + num, timelineGridAcc')
                    )
                    (0, timelineGrid)
                    adjacentPosistions

part2 :: Utils.SolutionSingle
part2 input =
  let (startPos, grid) = parsing input
   in modifiedDFS startPos grid

solution :: Utils.Solution
solution = (part1, part2)

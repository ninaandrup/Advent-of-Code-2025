module AoC.Day11 (solution) where

import Data.Bifunctor (second)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Utils.Utils as Utils

type NameMapping = Map.Map String Int

updateNameMapping :: Int -> String -> NameMapping -> NameMapping
updateNameMapping i name mapping =
  case Map.lookup name mapping of
    Just _ -> mapping
    Nothing -> Map.insert name i mapping

getNameIndex :: String -> NameMapping -> Int
getNameIndex name mapping =
  case Map.lookup name mapping of
    Just idx -> idx
    Nothing -> error $ "Name not found in mapping: " ++ name

type Graph = Seq.Seq [Int]

data NamedIndices = NamedIndices {startIdx :: Int, dacIdx :: Int, fftIdx :: Int} deriving (Eq, Show)

createNamedIndices :: String -> NameMapping -> NamedIndices
createNamedIndices startName mapping =
  NamedIndices
    { startIdx = Maybe.fromMaybe (-1) $ Map.lookup startName mapping,
      dacIdx = Maybe.fromMaybe (-1) $ Map.lookup "dac" mapping,
      fftIdx = Maybe.fromMaybe (-1) $ Map.lookup "fft" mapping
    }

parsing :: String -> [String] -> (NamedIndices, Graph)
parsing startName input =
  let pairs = Utils.splitInputToPair id ": " input
      edges = Seq.fromList $ map (second (splitOn " ")) pairs
      nameMapping = Seq.foldlWithIndex (\mapAcc i (name, _) -> updateNameMapping i name mapAcc) Map.empty edges
      nameMapping' = Map.insert "out" (Map.size nameMapping) nameMapping
      graph = Seq.mapWithIndex (\_ (_, neighbours) -> map (`getNameIndex` nameMapping') neighbours) edges
   in (createNamedIndices startName nameMapping', (Seq.|>) graph [])

data Status = White | Gray | Black Int deriving (Eq, Show)

dfs :: Int -> Graph -> Int
dfs start graph = fst $ dfsVisit start startStatuses
  where
    startStatuses =
      Seq.update (Seq.length graph - 1) (Black 1) $
        Seq.replicate (Seq.length graph) White

    dfsVisit :: Int -> Seq.Seq Status -> (Int, Seq.Seq Status)
    dfsVisit node statuses
      | Just color <- Seq.lookup node statuses = case color of
          (Black i) -> (i, statuses)
          Gray -> error "Cycle detected"
          White ->
            let statusesGreyNode = Seq.update node Gray statuses
                neighbours = graph `Seq.index` node
                (paths, newStatuses) = foldl foldF (0, statusesGreyNode) neighbours
             in (paths, Seq.update node (Black paths) newStatuses)
            where
              foldF :: (Int, Seq.Seq Status) -> Int -> (Int, Seq.Seq Status)
              foldF (countAcc, stsAcc) v =
                let (countV, stsV) = dfsVisit v stsAcc
                 in (countAcc + countV, stsV)
      | otherwise = error $ "Node index out of bounds: " ++ show node ++ " for statuses " ++ show statuses

part1 :: Utils.SolutionSingle
part1 input =
  let (namedIdx, graph) = parsing "you" input
   in dfs (startIdx namedIdx) graph

-- Device tuple: (dac, fft)
type Visited = Map.Map (Bool, Bool) Int

combineVisited :: Visited -> Visited -> Visited
combineVisited = Map.unionWith (+)

data StatusV2 = WhiteV2 | GrayV2 | BlackV2 Visited deriving (Eq, Show)

dfsV2 :: NamedIndices -> Graph -> Int
dfsV2 namedIdx graph = Maybe.fromMaybe 0 . Map.lookup (True, True) . fst $ dfsVisit (startIdx namedIdx) initStatuses
  where
    initStatuses :: Seq.Seq StatusV2
    initStatuses =
      Seq.update (Seq.length graph - 1) (BlackV2 $ Map.singleton (False, False) 1) $
        Seq.replicate (Seq.length graph) WhiteV2

    dfsVisit :: Int -> Seq.Seq StatusV2 -> (Visited, Seq.Seq StatusV2)
    dfsVisit node statuses
      | Just color <- Seq.lookup node statuses =
          case color of
            (BlackV2 visited) -> (visited, statuses)
            GrayV2 -> error "Cycle detected"
            WhiteV2 ->
              let statusesGreyNode = Seq.update node GrayV2 statuses
                  neighbours = graph `Seq.index` node
                  (visited, newStatuses) = foldl foldF (Map.empty, statusesGreyNode) neighbours
                  visited' = updateVisited node visited
               in (visited', Seq.update node (BlackV2 visited') newStatuses)
              where
                foldF :: (Visited, Seq.Seq StatusV2) -> Int -> (Visited, Seq.Seq StatusV2)
                foldF (visitedAcc, stsAcc) v =
                  let (visitedV, stsV) = dfsVisit v stsAcc
                   in (combineVisited visitedAcc visitedV, stsV)

                updateVisited :: Int -> Visited -> Visited
                updateVisited n
                  | n == dacIdx namedIdx = Map.mapKeys (\(_, fftVisited) -> (True, fftVisited))
                  | n == fftIdx namedIdx = Map.mapKeys (\(dacVisited, _) -> (dacVisited, True))
                  | otherwise = id
      | otherwise = error $ "Node index out of bounds: " ++ show node ++ " for statuses " ++ show statuses

part2 :: Utils.SolutionSingle
part2 input =
  let (namedIdx, graph) = parsing "svr" input
   in dfsV2 namedIdx graph

solution :: Utils.Solution
solution = (part1, part2)

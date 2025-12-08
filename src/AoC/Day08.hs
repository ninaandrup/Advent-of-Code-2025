module AoC.Day08 where

import qualified Data.Bifunctor
import Data.List (sortBy, sortOn)
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Utils.Utils as Utils

type Point = (Int, Int, Int)

type Points = V.Vector Point

type PointPairsSorted = V.Vector (Int, Int)

type PointCircuits = [Set.Set Int]

parsing :: [String] -> Points
parsing input = V.map toPoint . V.fromList $ Utils.splitInputToList read "," input
  where
    toPoint :: [Int] -> Point
    toPoint [x, y, z] = (x, y, z)
    toPoint _ = error "Invalid point format."

euclidianDistance :: Point -> Point -> Double
euclidianDistance (x1, y1, z1) (x2, y2, z2) =
  sqrt . fromIntegral $ ((x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2)

computePointPairsByDistances :: Points -> PointPairsSorted
computePointPairsByDistances points =
  let pairs = [(i, j) | i <- [0 .. V.length points - 1], j <- [i + 1 .. V.length points - 1]]
      pairsWithDist = map (\(i, j) -> ((i, j), euclidianDistance (points V.! i) (points V.! j))) pairs
      pairsWithDistSorted = V.fromList $ sortOn snd pairsWithDist
   in V.map fst pairsWithDistSorted

generateCircuits :: PointPairsSorted -> PointCircuits
generateCircuits = foldl (flip addPointToGroup) []
  where
    addPointToGroup :: (Int, Int) -> PointCircuits -> PointCircuits
    addPointToGroup p@(i, j) circuits =
      Set.insert i (Set.insert j connected) : rest
      where
        (connected, rest) = alreadyConnected p circuits

    alreadyConnected :: (Int, Int) -> PointCircuits -> (Set.Set Int, PointCircuits)
    alreadyConnected _ [] = (Set.empty, [])
    alreadyConnected p@(i, j) (circuit : rest)
      | Set.member i circuit || Set.member j circuit = (circuit `Set.union` connected, rest')
      | otherwise = (connected, circuit : rest')
      where
        (connected, rest') = alreadyConnected p rest

part1 :: Utils.SolutionSingle
part1 input =
  let points = parsing input
      pointPairs = computePointPairsByDistances points
      circuits = generateCircuits $ V.take connections pointPairs
      threeLargest = take 3 . sortBy (flip compare) $ map length circuits
   in product threeLargest
  where
    connections = if length input < 21 then 10 else 1000

generateCircuitsV2 :: Int -> PointPairsSorted -> (Int, Int)
generateCircuitsV2 len = generateCircuitsAux [] . V.toList
  where
    generateCircuitsAux :: PointCircuits -> [(Int, Int)] -> (Int, Int)
    generateCircuitsAux _ [] = error "No circuit found"
    generateCircuitsAux circuits (p : rest)
      | length newCircuits == 1 && Set.size (head newCircuits) == len = p
      | otherwise = generateCircuitsAux newCircuits rest
      where
        newCircuits = addPointToGroup p circuits

    addPointToGroup :: (Int, Int) -> PointCircuits -> PointCircuits
    addPointToGroup p@(i, j) circuits =
      Set.insert i (Set.insert j connected) : rest
      where
        (connected, rest) = alreadyConnected p circuits

    alreadyConnected :: (Int, Int) -> PointCircuits -> (Set.Set Int, PointCircuits)
    alreadyConnected _ [] = (Set.empty, [])
    alreadyConnected p@(i, j) (circuit : rest)
      | Set.member i circuit || Set.member j circuit = (circuit `Set.union` connected, rest')
      | otherwise = (connected, circuit : rest')
      where
        (connected, rest') = alreadyConnected p rest

part2 :: Utils.SolutionSingle
part2 input =
  let points = parsing input
      pointPairs = computePointPairsByDistances points
      lastTwoPoints = generateCircuitsV2 (V.length points) pointPairs
   in case Data.Bifunctor.bimap (points V.!) (points V.!) lastTwoPoints of
        ((x1, _, _), (x2, _, _)) -> x1 * x2

solution :: Utils.Solution
solution = (part1, part2)

{-# LANGUAGE TupleSections #-}

module AoC.Day09 (solution) where

import Data.List (maximumBy)
import qualified Data.Maybe as M
import Debug.Trace (trace)
import qualified Utils.Utils as Utils

type Point = (Int, Int)

pairs :: [a] -> [(a, a)]
pairs (x : xs) = map (x,) xs ++ pairs xs
pairs [] = []

area :: Point -> Point -> Int
area (x1, y1) (x2, y2) = width * height
  where
    width = abs (x2 - x1) + 1
    height = abs (y2 - y1) + 1

part1 :: Utils.SolutionSingle
part1 input =
  let points = Utils.splitInputToPair read "," input :: [(Int, Int)]
      pointPairs = pairs points
      areas = map (uncurry area) pointPairs
   in maximum areas

type HorizontalSegment = ((Int, Int), Int) -- ((x1, x2), y)

horizontalSegments :: [Point] -> [HorizontalSegment]
horizontalSegments [] = []
horizontalSegments [_] = []
horizontalSegments ((x1, y1) : p2@(x2, y2) : ps)
  | y1 == y2 = ((minX, maxX), y1) : horizontalSegments ps
  | otherwise = horizontalSegments (p2 : ps)
  where
    minX = min x1 x2
    maxX = max x1 x2

shootRayVerticallyFrom ::
  (Int -> Int -> Bool) -> -- direction predicate (compare segment y with point y)
  (Int -> Int -> Int) -> -- combine (min or max)
  Point ->
  [Point] ->
  Maybe Int
shootRayVerticallyFrom dirPred combine point allPoints = go Nothing point relevantSegments
  where
    relevantSegments = filter (\((_, _), y) -> dirPred y (snd point)) $ horizontalSegments allPoints

    go :: Maybe Int -> Point -> [HorizontalSegment] -> Maybe Int
    go acc _ [] = acc
    go acc p@(px, _) (((x1, x2), y) : qs)
      | x1 <= px && px <= x2 = go (Just (maybe y (combine y) acc)) p qs
      | otherwise = go acc p qs

shootRayUpwardsFrom :: Point -> [Point] -> Maybe Int
shootRayUpwardsFrom = shootRayVerticallyFrom (>) min

shootRayDownwardsFrom :: Point -> [Point] -> Maybe Int
shootRayDownwardsFrom = shootRayVerticallyFrom (<) max

type VerticalSegment = (Int, (Int, Int)) -- (x, (y1, y2))

verticalSegments :: [Point] -> [VerticalSegment]
verticalSegments [] = []
verticalSegments [_] = []
verticalSegments ((x1, y1) : p2@(x2, y2) : ps)
  | x1 == x2 = (x1, (minY, maxY)) : verticalSegments ps
  | otherwise = verticalSegments (p2 : ps)
  where
    minY = min y1 y2
    maxY = max y1 y2

findRightmostVerticalSegment :: VerticalSegment -> [Point] -> VerticalSegment
findRightmostVerticalSegment (px, (py1, py2)) allPoints = maximumBy (\(x1, _) (x2, _) -> compare x1 x2) relevantSegments
  where
    leftOfP :: VerticalSegment -> Bool
    leftOfP = (< px) . fst

    intersectingP :: VerticalSegment -> Bool
    intersectingP (_, (qy1, qy2)) = intersects qy1 || intersects qy2
      where
        intersects y = min py1 py2 < y && y < max py1 py2

    relevantSegments = filter (\seg -> leftOfP seg && intersectingP seg) $ verticalSegments allPoints

-- By visual inspection of the input,
-- part 2 must include either one of these points:
--  (94870, 50025) or (94870, 48753)
point1 :: Point
point1 = (94870, 50025)

point2 :: Point
point2 = (94870, 48753)

part2 :: Utils.SolutionSingle
part2 input = point2Area `max` point1Area
  where
    points = Utils.splitInputToPair read "," input :: [Point]

    point1MaxY = M.fromJust $ shootRayUpwardsFrom point1 points
    point1VerticalSegment = (fst point1, (snd point1, point1MaxY))
    (point1LeftSegmentX, point1LeftSegmentY) = findRightmostVerticalSegment point1VerticalSegment points
    point1LeftY = if snd point1LeftSegmentY < snd point1 then snd point1LeftSegmentY else fst point1LeftSegmentY
    point1Area = area point1 (point1LeftSegmentX, point1LeftY)

    point2MinY = M.fromJust $ shootRayDownwardsFrom point2 points
    point2VerticalSegment = (fst point2, (point2MinY, snd point2))
    (point2LeftSegmentX, point2LeftSegmentY) = findRightmostVerticalSegment point2VerticalSegment points
    point2LeftY = if snd point2 < snd point2LeftSegmentY then snd point2LeftSegmentY else fst point2LeftSegmentY
    point2Area = area point2 (point2LeftSegmentX, point2LeftY)

solution :: Utils.Solution
solution = (part1, part2)

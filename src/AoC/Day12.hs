module AoC.Day12 (solution) where

import Data.List.Split (splitOn)
import Debug.Trace (trace)
import qualified Utils.Utils as Utils

type Presents = [Present]

data Present = Present
  { shape :: Shape,
    area :: Int
  }
  deriving (Show)

type Shape = [[Bool]]

type Regions = [Region]

data Region = Region
  { wide :: Int,
    long :: Int,
    presentsCount :: [Int]
  }
  deriving (Show)

parsing :: [String] -> (Presents, Regions)
parsing input =
  ( map parsePresent presentsRaw,
    map parseRegion regionsRaw
  )
  where
    (presentsRaw, regionsRaw) = (\ls -> (init ls, last ls)) $ splitOn [""] input

    parsePresent :: [String] -> Present
    parsePresent presentStr =
      Present
        { shape = presentShape,
          area = length (concatMap (filter id) presentShape)
        }
      where
        presentShape = map (map (== '#')) $ tail presentStr

    parseRegion :: String -> Region
    parseRegion regionStr =
      Region
        { wide = w,
          long = l,
          presentsCount = counts
        }
      where
        (regionDimRaw, countsRaw) = Utils.splitInputToPairHelper id ": " regionStr
        (w, l) = Utils.splitInputToPairHelper read "x" regionDimRaw
        counts = map read $ splitOn " " countsRaw

data PreCheckResult
  = Infeasible
  | Solved
  | Unknown
  deriving (Eq)

regionArea :: Region -> Int
regionArea r = wide r * long r

presentsArea :: [Int] -> Presents -> Int
presentsArea count presents = sum $ zipWith (*) count (map area presents)

fitPresents :: Int -> Int -> Int
fitPresents w l =
  (w `div` 3) * (l `div` 3)

fitPreCheck :: Presents -> Region -> PreCheckResult
fitPreCheck presents region
  | regionArea region < presentsArea (presentsCount region) presents = Infeasible
  | sum (presentsCount region) <= fitPresents (wide region) (long region) = Solved
  | otherwise = trace ("UNKNOWN SOLUTION: " ++ show region) Unknown

part1 :: Utils.SolutionSingle
part1 input =
  length $ filter (== Solved) preCheckRes
  where
    (presents, regions) = parsing input
    preCheckRes = map (fitPreCheck presents) regions

part2 :: Utils.SolutionSingle
part2 _ = 0

solution :: Utils.Solution
solution = (part1, part2)

module AoC.Day06 (solution) where

import Data.List (transpose)
import Data.List.Split (splitOneOf, splitWhen)
import Debug.Trace (trace)
import qualified Utils.Utils as Utils

data Operand = Add | Multiply

data MathProblem = Math {operand :: Operand, values :: [Int]}

solveMathProblem :: MathProblem -> Int
solveMathProblem (Math Add vals) = sum vals
solveMathProblem (Math Multiply vals) = product vals

parsing :: [String] -> [MathProblem]
parsing = map toMathProblem . transpose . Utils.splitInputAtWhitespace id
  where
    toMathProblem :: [String] -> MathProblem
    toMathProblem input = case reverse input of
      ("+" : vals) -> Math {operand = Add, values = map read vals}
      ("*" : vals) -> Math {operand = Multiply, values = map read vals}
      _ -> error "Invalid math problem format."

solution1 :: Utils.SolutionSingle
solution1 = sum . map solveMathProblem . parsing

solution2 :: Utils.SolutionSingle
solution2 =
  sum
    . map (solveMathProblem . toMathProblem)
    . splitWhen (all (== ' '))
    . transpose
  where
    toMathProblem :: [String] -> MathProblem
    toMathProblem group =
      let oper = if last (head group) == '+' then Add else Multiply
          numbers = map read (init (head group) : tail group)
       in Math {operand = oper, values = numbers}

solution :: Utils.Solution
solution = (solution1, solution2)

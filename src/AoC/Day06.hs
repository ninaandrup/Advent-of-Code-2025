module AoC.Day06 (solution) where

import Data.List (transpose)
import Data.List.Split (splitOneOf)
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

parsingV2 :: [String] -> [MathProblem]
parsingV2 input =
  let operandStr = last input
      operands = map (\o -> if o == "+" then Add else Multiply) $ words operandStr

      splitNumbers = map (splitNumbersBeforeOperands operandStr) $ init input
      colNumbers = map transpose $ transpose splitNumbers
   in toMathProblem operands colNumbers
  where
    splitNumbersBeforeOperands :: String -> String -> [String]
    splitNumbersBeforeOperands opers str =
      let shiftOpers = tail opers ++ [' ']
       in splitOneOf "+*" $
            zipWith (\o c -> if o /= ' ' then o else c) shiftOpers str
    toMathProblem :: [Operand] -> [[String]] -> [MathProblem]
    toMathProblem = zipWith (\o c -> Math {operand = o, values = map read c})

solution2 :: Utils.SolutionSingle
solution2 = sum . map solveMathProblem . parsingV2

solution :: Utils.Solution
solution = (solution1, solution2)

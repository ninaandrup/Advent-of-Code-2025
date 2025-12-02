module AoC.Day01 (solution) where

import qualified Utils

parsing :: String -> Either Int Int
parsing ('L' : num) = Left (read num :: Int)
parsing ('R' : num) = Right (read num :: Int)
parsing _ = error "Invalid input."

leftRotate :: Int -> Int
leftRotate x
  | x < 0 = leftRotate (x + 100)
  | otherwise = x

rightRotate :: Int -> Int
rightRotate x
  | x > 99 = rightRotate (x - 100)
  | otherwise = x

rotate :: Int -> Either Int Int -> Int
rotate dialPos (Left rotateBy) = leftRotate $ dialPos - rotateBy
rotate dialPos (Right rotateBy) = rightRotate $ dialPos + rotateBy

updateDialPos :: (Int, Int) -> Either Int Int -> (Int, Int)
updateDialPos (dialPos, zeroCount) next =
  let newDialPos = rotate dialPos next
   in if newDialPos == 0 then (newDialPos, zeroCount + 1) else (newDialPos, zeroCount)

solution1 :: Utils.SolutionSingle
solution1 input =
  let instructions = map parsing input
   in snd $ foldl updateDialPos (50, 0) instructions

rotateV2 :: Int -> Int -> Either Int Int -> (Int, Int)
rotateV2 dialPos zeroCount (Left rotateBy)
  | rotateBy == 0 = (dialPos, zeroCount)
  | rotateBy > 0 && dialPos == 0 =
      let newDialPos = 99
       in rotateV2 newDialPos zeroCount (Left (rotateBy - 1))
  | rotateBy > 0 =
      let newDialPos = dialPos - 1
          newZeroCount = if newDialPos == 0 then zeroCount + 1 else zeroCount
       in rotateV2 newDialPos newZeroCount (Left (rotateBy - 1))
  | otherwise = error "Negative rotation not supported."
rotateV2 dialPos zeroCount (Right rotateBy)
  | rotateBy == 0 = (dialPos, zeroCount)
  | rotateBy > 0 && dialPos == 99 =
      let newDialPos = 0
          newZeroCount = zeroCount + 1
       in rotateV2 newDialPos newZeroCount (Right (rotateBy - 1))
  | rotateBy > 0 =
      let newDialPos = dialPos + 1
          newZeroCount = if newDialPos == 0 then zeroCount + 1 else zeroCount
       in rotateV2 newDialPos newZeroCount (Right (rotateBy - 1))
  | otherwise = error "Negative rotation not supported."

updateDialPosV2 :: (Int, Int) -> Either Int Int -> (Int, Int)
updateDialPosV2 (dialPos, zeroCount) = rotateV2 dialPos zeroCount

solution2 :: Utils.SolutionSingle
solution2 input =
  let instructions = map parsing input
   in snd $ foldl updateDialPosV2 (50, 0) instructions

solution :: Utils.Solution
solution = (solution1, solution2)

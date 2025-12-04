module Utils.Grid where

import Data.Bifunctor (second)
import qualified Data.Vector as V

-- Row-major 2D grid
-- [[a1, a2, a3, a4],
--  [b1, b2, b3, b4],
--  [c1, c2, c3, c4]] =
-- -- Grid { width = 4, height = 3,
--           cells = V.fromList [a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4] }
data Grid a = Grid
  { width :: Int,
    height :: Int,
    cells :: V.Vector a
  }

indexGrid :: Int -> Int -> Int -> Int
indexGrid w row col = row * w + col

lookupGrid :: Int -> Int -> Grid a -> Maybe a
lookupGrid row col grid
  | row < 0 || col < 0 = Nothing
  | h - 1 < row = Nothing
  | w - 1 < col = Nothing
  | otherwise = cells grid V.!? i
  where
    w = width grid
    h = height grid
    i = indexGrid w row col

mapGrid :: (a -> b) -> Grid a -> Grid b
mapGrid f grid =
  Grid
    { width = width grid,
      height = height grid,
      cells = V.map f (cells grid)
    }

countGrid :: (a -> Bool) -> Grid a -> Int
countGrid predicate grid = V.length $ V.filter predicate (cells grid)

getGridWithPos :: Grid a -> Grid (a, (Int, Int))
getGridWithPos grid =
  let pos = V.fromList [(r, c) | r <- [0 .. height grid - 1], c <- [0 .. width grid - 1]]
   in Grid
        { width = width grid,
          height = height grid,
          cells = V.zip (cells grid) pos
        }

fromListGrid :: [[a]] -> Grid a
fromListGrid grid
  | null grid = Grid {width = 0, height = 0, cells = V.empty}
  | otherwise =
      Grid
        { width = length (head grid),
          height = length grid,
          cells = V.fromList (concat grid)
        }

adjecentPositions :: Int -> Int -> V.Vector (Int, Int)
adjecentPositions row col =
  let deltas =
        [ (-1, -1),
          (-1, 0),
          (-1, 1),
          (0, -1),
          (0, 1),
          (1, -1),
          (1, 0),
          (1, 1)
        ]
      adjecentPos = [(row + dr, col + dc) | (dr, dc) <- deltas]
   in V.fromList adjecentPos

adjecentCount :: (a -> Bool) -> Grid a -> Grid (a, Int)
adjecentCount predicate grid =
  let gridWithAdjecentPos = mapGrid (second (uncurry adjecentPositions)) $ getGridWithPos grid
      gridWithAdjecentVals = mapGrid (second (V.map (\(r, c) -> lookupGrid r c grid))) gridWithAdjecentPos
      predicateF = maybe False predicate
      gridWithAdjecentCount = mapGrid (second (V.length . V.filter id . V.map predicateF)) gridWithAdjecentVals
   in gridWithAdjecentCount

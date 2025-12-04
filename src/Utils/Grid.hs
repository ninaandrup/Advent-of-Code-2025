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

instance Functor Grid where
  fmap f grid = Grid {width = width grid, height = height grid, cells = V.map f (cells grid)}

instance (Show a) => Show (Grid a) where
  show grid = show (width grid, height grid, show (cells grid))

getAbove :: Int -> Grid a -> Maybe a
getAbove idx grid = cells grid V.!? (idx - width grid)

getBelow :: Int -> Grid a -> Maybe a
getBelow idx grid = cells grid V.!? (idx + width grid)

getValidAdjacentIndices :: Int -> Grid a -> V.Vector Int
getValidAdjacentIndices idx grid =
  let isLeftEdge = idx `mod` width grid == 0
      isRightEdge = (idx + 1) `mod` width grid == 0

      left = if isLeftEdge then [] else [above - 1, idx - 1, below - 1]
      right = if isRightEdge then [] else [above + 1, idx + 1, below + 1]

      adjacentIndices = [above, below] ++ left ++ right
   in V.filter (\i -> 0 <= i && i < len) (V.fromList adjacentIndices)
  where
    above = idx - width grid
    below = idx + width grid
    len = V.length (cells grid)

getAdjacent :: Int -> Grid a -> V.Vector a
getAdjacent idx grid = V.map (\i -> cells grid V.! i) (getValidAdjacentIndices idx grid)

fromGridList :: [[a]] -> Grid a
fromGridList grid
  | null grid = Grid {width = 0, height = 0, cells = V.empty}
  | otherwise =
      Grid
        { width = length (head grid),
          height = length grid,
          cells = V.fromList (concat grid)
        }

adjacentCount :: (a -> Bool) -> Grid a -> Grid (a, Int)
adjacentCount predicate grid =
  let adjacentElems = V.imap (\i v -> (v, getAdjacent i grid)) $ cells grid
      adjacentCountVec = V.map (second (length . V.filter id . V.map predicate)) adjacentElems
   in Grid {width = width grid, height = height grid, cells = adjacentCountVec}

countGrid :: (a -> Bool) -> Grid a -> Int
countGrid predicate = V.length . V.filter predicate . cells

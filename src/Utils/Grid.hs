module Utils.Grid where

import qualified Data.Vector as V

-- Row-major 2D grid
-- [[a00, a01, a02, a03],
--  [a10, a11, a12, a13],
--  [a20, a21, a22, a23]] =
-- -- Grid { width = 4, height = 3,
--           cells = V.fromList [a00, a01, a02, a03, a10, a11, a12, a13, a20, a21, a22, a23] }
data Grid a = Grid
  { width :: Int,
    height :: Int,
    cells :: V.Vector a
  }

instance Functor Grid where
  fmap f grid = Grid {width = width grid, height = height grid, cells = V.map f (cells grid)}

instance (Show a) => Show (Grid a) where
  show grid = show (width grid, height grid, show (cells grid))

fromGridList :: [[a]] -> Grid a
fromGridList grid
  | null grid = Grid {width = 0, height = 0, cells = V.empty}
  | otherwise =
      Grid
        { width = length (head grid),
          height = length grid,
          cells = V.fromList (concat grid)
        }

-- Convert (x, y) coordinates to (row, column) index
--
-- [[a00, a01, a02, a03],
--  [a10, a11, a12, a13],
--  [a20, a21, a22, a23]]
--
-- (0,0) -> a00
-- (0,1) -> a01
-- (1,0) -> a10
-- (2,3) -> a23
getCell :: (Int, Int) -> Grid a -> a
getCell (x, y) grid = cells grid V.! (x * width grid + y)

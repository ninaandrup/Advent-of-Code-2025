module Utils.Grid where

import qualified Data.Set as Set
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
  show = concatMap ((++ "\n") . show) . showGrid

showGrid :: (Show a) => Grid a -> V.Vector (V.Vector a)
showGrid grid =
  V.fromList [V.slice (i * width grid) (width grid) (cells grid) | i <- [0 .. height grid - 1]]

emptyGrid :: Grid a
emptyGrid = Grid {width = 0, height = 0, cells = V.empty}

fromGridList :: [[a]] -> Grid a
fromGridList grid
  | null grid = Grid {width = 0, height = 0, cells = V.empty}
  | otherwise =
      Grid
        { width = length (head grid),
          height = length grid,
          cells = V.fromList (concat grid)
        }

type CellPos = (Int, Int)

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
getCell :: CellPos -> Grid a -> a
getCell (x, y) grid = cells grid V.! (x * width grid + y)

getCellSafe :: CellPos -> Grid a -> Maybe a
getCellSafe (x, y) grid
  | validCell (x, y) grid = Just $ getCell (x, y) grid
  | otherwise = Nothing

validCell :: CellPos -> Grid a -> Bool
validCell (x, y) grid
  | height grid <= x = False
  | y < 0 = False
  | width grid <= y = False
  | otherwise = True

type Visited = Set.Set CellPos

type Adjacent = (CellPos -> [CellPos])

dfs :: CellPos -> Adjacent -> Visited
dfs start adj = dfsIter start Set.empty
  where
    dfsIter :: CellPos -> Visited -> Visited
    dfsIter cellPos visited =
      foldl
        ( \acc adjPos ->
            if Set.member adjPos acc
              then acc
              else dfsIter adjPos acc
        )
        (Set.insert cellPos visited)
        $ adj cellPos

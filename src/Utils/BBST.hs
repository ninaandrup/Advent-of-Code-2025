module Utils.BBST where

import Data.List (sortBy)

data BBST k v
  = Empty
  | Node (k, v) (BBST k v) (BBST k v)
  deriving (Show, Eq)

fromListAux :: [(k, v)] -> BBST k v
fromListAux [] = Empty
fromListAux ls =
  let mid = length ls `div` 2
   in case splitAt mid ls of
        (left, []) -> fromListAux left
        (left, x : right) ->
          Node x (fromListAux left) (fromListAux right)

fromList :: (Ord k) => [(k, v)] -> BBST k v
fromList = fromListAux . sortBy (\(a, _) (b, _) -> compare a b)

predecessor :: (Ord k) => k -> BBST k v -> Maybe (k, v)
predecessor _ Empty = Nothing
predecessor i (Node (k, v) left right)
  | i == k = Just (k, v)
  | i < k = predecessor i left
  | otherwise = case predecessor i right of
      Just kv -> Just kv
      Nothing -> Just (k, v)

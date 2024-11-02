module Median (median, medianBy, select, selectBy) where

import Data.List (sortBy)
import Data.List.Split (chunksOf)

-- select xs k == (sort xs) !! k
select :: Ord a => [a] -> Int -> a
select = selectBy compare

median :: Ord a => [a] -> a
median = medianBy compare

medianBy :: (a -> a -> Ordering) -> [a] -> a
medianBy _ [] = error "Can't find median of empty list"
medianBy cmp xs
  | length xs <= 5 = sortBy cmp xs !! ((length xs - 1) `div` 2)
  | otherwise = let medians = map (medianBy cmp) (chunksOf 5 xs) in
        selectWithPivotBy cmp xs (medianBy cmp medians) ((length xs - 1) `div` 2)

selectWithPivotBy :: (a -> a -> Ordering) -> [a] -> a -> Int -> a
selectWithPivotBy cmp xs pivot k
  | k < length smaller = selectBy cmp smaller k
  | k < length smaller + length equal = pivot
  | otherwise = selectBy cmp larger (k - length smaller - length equal)
    where smaller = filter (\x -> x `cmp` pivot == LT) xs
          equal   = filter (\x -> x `cmp` pivot == EQ) xs
          larger  = filter (\x -> x `cmp` pivot == GT) xs

selectBy :: (a -> a -> Ordering) -> [a] -> Int -> a
selectBy _ []  _ = error "Can't select from empty list"
selectBy _ [x] 0 = x
selectBy _ [_] _ = error "Failed"
selectBy cmp (x:xs) k = selectWithPivotBy cmp (x:xs) goodPivot k
    where goodPivot = medianBy cmp (x:xs)
-- median -> selectWithPivot -> select -> selectWithPivot,median

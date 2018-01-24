{-# LANGUAGE TypeInType, TypeOperators, TypeFamilies #-}

module Sorting ( mergeSort, heapSort
               , quickSort, insertionSort
               , Sorted, sortedBy) where

import ONotation
import Data.List (sort)


-- We encode in the return type of the sorting function its average complexity,
-- memory use and stability.
newtype Sorted (avg :: AsymptoticPoly) (mem :: AsymptoticPoly) (stable :: Bool) a
  = Sorted {sortedBy :: [a]}

mergeSort :: (Ord a, n >=. O(N*.LogN), m >=. O(N)) => [a] -> Sorted n m True a
mergeSort = Sorted . sort

quickSort :: (Ord a, n >=. O(N*.LogN), m >=. O(N)) => [a] -> Sorted n m False a
quickSort = Sorted . sort
  
heapSort :: (Ord a, n >=. O(N*.LogN), m >=. O(One)) => [a] -> Sorted n m False a
heapSort = Sorted . sort

insertionSort :: (Ord a, n >=. O(N^.2), m >=. O(One)) => [a] -> Sorted n m True a
insertionSort = Sorted . sort

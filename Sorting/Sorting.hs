{-# LANGUAGE TypeInType, TypeOperators, TypeFamilies, ConstraintKinds #-}

module Sorting ( mergeSort, heapSort
               , quickSort, insertionSort
               , Sorted, sortedBy) where

import Data.Type.Bool
import ONotation
import Data.List (sort)


-- Stable sorts must be stable, but unstable can be, but don't need to.
type IsStable s = (s || True) ~ True
-- We encode in the return type of the sorting function its average complexity,
-- memory use and stability.
newtype Sorted (cpu :: AsymptoticPoly) -- The minimum operational complexity
                                       -- this algorithm satisfies.
               (mem :: AsymptoticPoly) -- The minimum space complexity this
                                       -- algorithm satisfies.
               (stable :: Bool)        -- Whether the sort is stable or not.
               a                       -- What was being sorted.
               = Sorted {sortedBy :: [a]}

-- Merge sort is O(N*Log(N)) on average in complexity, so that's the
-- minimum complexity we promise to satisfy. Same goes with memory, which is
-- O(N), and as we all know, mergesort is a stable sorting algoritm.
mergeSort :: (Ord a, n >=. O(N*.LogN), m >=. O(N), IsStable s) => [a] -> Sorted n m s a
mergeSort = Sorted . sort

insertionSort :: (Ord a, n >=. O(N^.2), m >=. O(One), IsStable s) => [a] -> Sorted n m s a
insertionSort = Sorted . sort

-- Note that we don't actually check the complexity (as evidenced by them all
-- being implemented with sort, a smooth applicative merge sort). With more
-- dependent types however, some of these properties might be verifiable.
quickSort :: (Ord a, n >=. O(N*.LogN), m >=. O(N)) => [a] -> Sorted n m False a
quickSort = Sorted . sort
  
heapSort :: (Ord a, n >=. O(N*.LogN), m >=. O(One)) => [a] -> Sorted n m False a
heapSort = Sorted . sort


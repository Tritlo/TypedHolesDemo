{-# LANGUAGE TypeInType, TypeOperators #-}

module Main where

import Sorting
import ONotation

-- Here we say that sorted can use at most operational complexity O(N^2), space
-- complexity of at most (O(N)) and that it should be stable.
mySortA :: Sorted (O(N^.2)) (O(N)) True Integer
mySortA = _a [3,1,2]

mySortB :: Sorted (O(N*.LogN)) (O(N)) False Integer
mySortB = _b [3,1,2]

mySortC :: Sorted (O(N*.LogN)) (O(One)) False Integer
mySortC = _c [3,1,2]

main :: IO ()
main = print (sortedBy mySortA)

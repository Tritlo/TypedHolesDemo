{-# LANGUAGE TypeInType, TypeOperators #-}

module Main where

import Sorting
import ONotation

-- Here we say that sorted can use at most operational complexity O(N^2), space
-- complexity of at most (O(N)) and that it should be stable.
mySort :: Sorted (O(N^.2)) (O(N)) True Integer
mySort = _ [3,1,2]


main :: IO ()
main = print (sortedBy mySort)

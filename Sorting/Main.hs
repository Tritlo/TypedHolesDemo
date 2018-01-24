{-# LANGUAGE TypeInType, TypeOperators #-}

module Main where

import Sorting
import ONotation

-- Here we say that sorted can use at most complexity N^2 and at most memory N^1
-- and that the sort has to be stable.
mySort :: Sorted (O(N^.2)) (O(N)) True Integer
mySort = _ [3,1,2]


main :: IO ()
main = print (sortedBy mySort)

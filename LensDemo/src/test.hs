{-# OPTIONS_GHC -fno-max-valid-substitutions #-}
-- Consider the following module
module Main where

-- We import everyones favorite library
import Control.Lens

-- And we declare a very simple data type
newtype Test = Test { _value :: Integer } deriving (Show)

-- We declare a lens to be able to operate on that data type
value :: Lens' Test Integer
value f (Test i) = Test <$> f i

-- But, we want to try the newfangled do-notation syntax we've heard so
-- much about!

-- Let's try it!
updTest :: Test -> Test
updTest t = t &~ do
    -- Ok, so we want to update the value by one.
    -- _ 1 value
    -- Hmm. Nothing found. How about
    _ value 1
    -- Ah, there we go. Yes of course, the
    -- <<+= operator was the one I was looking for,
    -- thanks!

-- I don't know lens well enough to show more cool examples. Time to learn,
-- I guess

emptyTest :: Test
emptyTest = Test 0

main :: IO ()
main =  print (updTest emptyTest)

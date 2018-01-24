module Main where

import Control.Lens

newtype Test = Test { _value :: Integer } deriving (Show)

value :: Lens' Test Integer
value f (Test i) = Test <$> f i

updTest :: Test -> Test
updTest t = t &~ do
  --_ value (+1)
  value += 1

emptyTest :: Test
emptyTest = Test 0

main :: IO ()
main =  print (updTest emptyTest)

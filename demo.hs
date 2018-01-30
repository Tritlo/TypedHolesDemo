module Demo where
import Control.Monad
import Control.Applicative
import Data.List hiding (concat, map)
import Prelude hiding (sum, concat, map)
import Data.Char (isUpper, isLower)

-- Consider the function.
-- We're wondering, how can we make this string into a list of string?

-- f :: [String]
-- f = _ "hello, world"


-- main :: IO ()
-- main = print $ take 10 f

-- Let's try it!
--
-- Wow, so many possibilities! But what is mempty?
-- It's the Monoid b => Monoid (a -> b) instance, with mempty = const mempty!

-- That was neat. But what about TWO holes at the same time? That's what I
-- would do if I had a million doll-, uh I mean a valid substitution suggester!

-- f :: [String]
-- f = _a _b "hello, world"

-- main :: IO () 
-- main = return ()

-- Oof, that was a bit slow, and way too much output. We can speed it up by
-- passing -fno-sort-valid-substitutions, and then it won't put any work into
-- sorting the output.

-- Let's try again, with more specific types. Let's say I'm looking for a way
-- to change the String, assuming I have some function to compare elements.
-- What functions can I use? And what functions can I use to change the list?

-- Let's try it!
-- f2 :: String 
-- f2 = _a (_b :: Char -> Bool) "hello, world"

-- Aha! We can filter, takeWhile, dropWhile, we can sortOn and all kinds of
-- things.

-- But Matti, you say. That's all really cool, but how does it handle
-- Type classes The answer: Pretty well! Let's try it. Here we have
-- the classic map function over a list. But how can we implement it?
-- Let's try it!

-- myMap :: (a -> b) -> [a] -> [b]
-- myMap = _

-- Aha! map can be written with fmap! But what about this?
-- If I have a list of functions and a list of things, how can I get
-- the result of applying those functions? Let's ask GHC with a hole

-- applyFunctions :: [a -> b] -> [a] -> [b]
-- applyFunctions = _

-- We can use (<*>). Neat!
-- How can we write concat with a more general function?
-- concatenateLists :: [[a]] -> [a]
-- concatenateLists = _

-- Nice!
-- What about sum?

-- mySum :: [Integer] -> Integer
-- mySum = _ (+) 0

--And prod?
-- prod :: [Integer] -> Integer
-- prod = foldl _ 1

-- Wow, so many possibilities!

-- But what about constraints? Well, that works too! Let's try it.
-- What are the things in scope that we can show?

-- h :: String
-- h = show _

-- Neat!

-- But what about many at once? Of course!
-- h2 :: String
-- h2 = show (_ ++ [])

-- Oh, only the Strings in scope are those lists which we can show. Oh well.


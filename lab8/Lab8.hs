module Lab8 where

-- Translate the following Scheme expressions into Haskell.

-- (define life 42)
life = 42

-- (define life' (+ 42 24))
life' = (+) 42 24
-- assert the types of life and life' 
life :: Integer
life' :: Integer


-- (define (implies a b) (if a b True))
-- assert the type of implies
implies a b = if a then b else True
implies :: Bool -> Bool -> Bool

-- (define (greet x) (string-append "hello, " x))
-- assert the type of greet
greet x = (++) "hello, " x
greet :: [Char] -> [Char]

-- define a function stutter that repeats each element in the given list twice
-- assert the type of stutter
stutter lst = if lst == [] then [] else head lst : head lst : stutter (tail lst)
stutter :: Eq a => [a] -> [a]

-- now let's use pattern matching to produce a much better solution
-- call it stutter'
-- assert the type of stutter'
stutter' [] = [] 
stutter' (x:xs) = x:x:stutter' xs
stutter' :: Eq a => [a] -> [a]

-- define len (length of list) using pattern matching
-- assert the type of len
len [] = 0
len (x:xs) = 1 + len xs
len :: Eq a => [a] -> Int

-- define firsts that takes a list of pairs and returns a list of its first elements
-- use pattern mathing
-- assert the type of firsts
firsts [] = []
firsts ((x, y):xs) = x:firsts xs
firsts :: [(a, b)] -> [a]

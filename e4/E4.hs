module E4 where

-- |interleave (xs, ys) 
-- return a list of corresponding elements from xs and ys, 
-- interleaved. The remaining elements of the longer list
-- are ignored.
interleave ([], []) = []
interleave (xs, []) = []
interleave ([], ys) = []
interleave (xs, ys) = head xs:head ys:interleave (tail xs, tail ys)

-- |toPairs (xs, ys) 
-- return a list of pairs of corresponding elements from xs and ys, 
-- interleaved. The remaining elements of the longer list
-- are ignored.
toPairs ([], []) = []
toPairs (xs, []) = []
toPairs ([], ys) = []
toPairs (xs, ys) = (head xs, head ys):toPairs (tail xs, tail ys)

-- |repeat' (f, x, n)
-- return a list [x, f(x), f(f(x)), ..., f^n(x)]
-- reuires n >= 0
temp' (f, x, 0) = x
temp' (f, x, n) = f (temp' (f, x, n - 1))

repeat' (f, x, n) = [temp' (f, x, l) | l <- [0..n]]

-- |numNeg xs 
-- return a number of negative elements in xs
--  Use list comprehension!
numNeg xs = sum [1 | x <- xs, x < 0]


-- |genSquares (low, high)
-- return a list of squares of even integers between low and high, inclusive.
-- Use list comprehension!
genSquares (low, high) = [ x^2 | x <- [low..high], even x]

-- |triples n 
-- return a list of triples (x,y,z) all less than or equal to n, such
-- that x^2 + y^2 == z^2, with no duplicate triples or premutations of
-- ealier triples.
-- Use list comprehension!
triples n = [ (x, y, z) | x <- [1..n], y <- [x..n], z <- [y..n], x^2 + y^2 == z^2]

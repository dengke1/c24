module SetEq where

import Set
import Data.List

contains a [] = False
contains a (x:xs) = if a == x then True else contains a xs

subset a [] = False
subset [] (y:ys) = True
subset [x] (y:ys) = contains x (y:ys)
subset (x:xs) (y:ys) = if contains x (y:ys) then subset xs (y:ys) else False

instance Ord a  => Eq (Set a) where
	(Set []) == (Set a) = [] == a
	(Set a) == (Set []) = a == []
	(Set [a]) == (Set [b]) = a == b
	(Set (x:xs)) == (Set (y:ys)) = length (x:xs) == length (y:ys) && subset (x:xs) (y:ys)

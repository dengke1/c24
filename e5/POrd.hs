module POrd where

import Set

class POrd a where
	(gte), (lte), (lt), (gt) :: a -> a -> Bool
	pcompare :: a -> a -> POrdering

	x 'lt' y = False
	(x:xs) 'lt' (y:ys) = length (x:xs) < length (x:xs)
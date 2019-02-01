module SetShow where

import Set


instance Show a => Show (Set a) where
	show (Set []) = "{}"
	show (Set [a]) = "{" ++ show a ++ "}"
	show (Set (x:xs)) = "{" ++ tail (init (show (x:xs))) ++ "}"
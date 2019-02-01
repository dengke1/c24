condApply _ _ [] = []
condApply p f (x:xs) | p x = f x:condApply p f xs
					| otherwise = x:condApply p f xs



condFoldr p f xs = foldr (\x l -> if p x then f x:l else x:l) [] xs
condFoldr :: (a -> Bool) -> (a -> a) -> [a] -> [a]


condAll ps f xs = foldr (\x l -> if notall x ps then f x:l else x:l) [] xs
where notall v ps = foldr (\p c -> p v && c) True ps


condAll :: [(a -> Bool)] -> (a -> a) -> [a] -> [a]
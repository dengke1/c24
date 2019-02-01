data BTree a = Empty | BTree a (BTree a) (BTree a) deriving Show

bst = BTree 5
	(BTree 3 (BTree 1 Empty Empty) (BTree 4 Empty Empty))
	(BTree 7 (BTree 6 Empty Empty) (BTree 10 Empty Empty))

bst' = BTree 6
	(BTree 3 (BTree 1 Empty Empty) (BTree 4 Empty (BTree 5 Empty Empty)))
	(BTree 7 Empty (BTree 10 Empty Empty))

bst'' = BTree 6
	(BTree 9 (BTree 1 Empty Empty) (BTree 4 Empty (BTree 5 Empty Empty)))
	(BTree 7 Empty (BTree 10 Empty Empty))

numNodes Empty = 0
numNodes (BTree a l r) = 1 + numNodes l + numNodes r
numNodes:: (BTree a) -> Int

inorder Empty = []
inorder (BTree a l r) = inorder l ++ [a] ++ inorder r
inorder:: (BTree a) -> [a]

sorted [] = True
sorted [x] = True
sorted (x:y:xs) = x <= y && sorted (y:xs)
sorted:: Ord a => [a] -> Bool

isBST Empty = True
isBST (BTree a Empty Empty) = True
isBST (BTree a (BTree l ll lr) Empty) = isBST (BTree l ll lr) && a >= l
isBST (BTree a Empty (BTree l ll lr)) = isBST (BTree l ll lr) && a <= l
isBST (BTree a (BTree l ll lr) (BTree r rl rr)) = isBST (BTree l ll lr) && isBST (BTree r rl rr) && a >= l && a <= r
isBST:: Ord a => (BTree a) -> Bool


instance Eq a => Eq (BTree a) where
	b == b' = inorder b == inorder b'
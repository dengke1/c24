module Trees where

-- A funny tree.
data Tree a = Leaf a | Node  (a, Tree a, Tree a)


--five = Node (1, 
--               Node (2, Leaf 3, Leaf 4),
--               Leaf 5)

-- countNodes t
-- return the number of nodes in tree t
countNodes (Leaf a) = 1
countNodes (Node (a, b, c)) = 1 + countNodes b + countNodes c

-- forallNodes p t
-- return whether p is true of every node in tree t
forallNodes p (Leaf a) = p a
forallNodes p (Node (a, b, c)) = p a && forallNodes p b && forallNodes p c

-- existsNode p t
-- return whether p is true of some node in tree t
existsNode p (Leaf a) = p a
existsNode p (Node (a, b, c)) = p a || existsNode p b || existsNode p c
-- inorder t
-- return a list of nodes in t in inorder traversal order
inorder (Leaf a) = [a]
inorder (Node (a, b, c)) = inorder b ++ [a] ++ inorder c

tfold f g (Leaf x) = f x
tfold f g (Node (x, left, right)) = 
    g x (tfold f g left) (tfold f g right)

tfold:: (a -> b) -> (a -> b -> b -> b) -> Tree a -> b

countNodes' = tfold (const 1) (\ _ x y -> x + y + 1)

forallNodes' p = tfold p (\ x l r -> p x && l && r)

existsNode' p = tfold p (\ x l r -> p x || l || r)

inorder' = tfold (: []) (\ x l r -> l ++ [x] ++ r)

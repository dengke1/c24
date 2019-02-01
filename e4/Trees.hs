module Trees where

-- A funny tree.
data Tree a = Leaf a | Node  (a, Tree a, Tree a)

-- countNodes t
-- return the number of nodes in tree t
countNodes  = 42

-- forallNodes p t
-- return whether p is true of every node in tree t
forallNodes = 42

-- existsNode p t
-- return whether p is true of some node in tree t
existsNode = 42

-- inorder t
-- return a list of nodes in t in inorder traversal order
inorder = 42

tfold f g (Leaf x) = f x
tfold f g (Node (x, left, right)) = 
    g x (tfold f g left) (tfold f g right)

tfold:: (a -> b) -> (a -> b -> b -> b) -> Tree a -> b

countNodes' = 42

forallNodes' = 42

existsNode' = 42

inorder' = 42

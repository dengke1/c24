module Lab9 where

-- mathematical expressions
-- leafs are numbers (integers for now)
-- internal nodes are either unary functions and one child
-- or binary functions and two children
data MathTree = Leaf Int
              | Unary (Int -> Int, MathTree)
              | Binary ((Int, Int) -> Int, MathTree, MathTree)

-- This is here to let Haskell know how to display the MathTree to you.
-- We will learn later what exactly this is and how it works.
instance Show MathTree where
    show (Leaf v) = show v
    show (Unary (f,t)) = "unary(" ++ show t ++ ")"
    show (Binary (f,l,r)) = "binary(" ++ show l ++ "," ++ show r ++ ")"

-- evaluate the math tree
eval :: MathTree -> Int

eval (Leaf num) = num
eval (Unary (f, t)) = f (eval t)
eval (Binary (f, a, b)) = f ((eval a), (eval b))

-- apply (op, t)
-- apply op to every leaf in t
apply :: ((Int -> Int), MathTree) -> MathTree

apply (op, Leaf v) = Leaf (op v)
apply (op, Unary (f, t)) = Unary (f, apply (op, t))
apply (op, Binary (f, a, b)) = Binary (f, apply (op, a), apply (op, b))

-- search (v, t)
-- return whether v is in tree t
search :: (Int, MathTree) -> Bool

search (num, Leaf x) = x == num
search (num, Unary (f, t)) = search (num, t)
search (num, Binary (f, a, b)) = search (num, a) || search (num, b)

-- replace (v, v', t)
-- replace every value v in the leafs with v'
replace :: (Int, Int, MathTree) -> MathTree

replace (a, z, Leaf x) = if x == a then (Leaf z) else (Leaf x)
replace (a, z, Unary (f, t)) = Unary(f, replace (a, z, t))
replace (a, z, Binary (f, t, r)) = Binary (f, replace (a, z, t), replace (a, z, r))

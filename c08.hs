-- c8e1
--- In a similar manner to the function add, define a recursive
--- multiplication function mult :: Nat -> Nat -> Nat for the
--- recursive type of natural numbers.
data Nat = Zero | Succ Nat
  deriving Show

addNat :: Nat -> Nat -> Nat
addNat n Zero = n
addNat n (Succ m) = Succ (addNat n m)

multNat :: Nat -> Nat -> Nat
multNat _ Zero = Zero
multNat n (Succ m) = addNat (multNat n m) n

-- c8e2
--- Using the function compare, redefine the function occurs :: Ord a
--- => a -> Tree a -> Bool for search trees. Why is this new
--- definition more efficient than the original version?
data Tree a = Leaf a | Node (Tree a) a (Tree a)
  deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) | compare x y == LT = occurs x l
                      | compare x y == GT = occurs x r
                      | otherwise = occurs x (Leaf y)

-- c8e3
--- Let us say that such a tree is balanced if the number of leaves in
--- the left and right subtree of every node differs by at most one,
--- with leaves themselves being trivially balanced. Define a function
--- balanced :: Tree a -> Bool that decides if a binary tree is
--- balanced or not.
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)
  deriving Show

countLeaves :: Tree' a -> Int
countLeaves (Leaf' y) = 1
countLeaves (Node' l r) = countLeaves l + countLeaves r

balanced :: Tree' a -> Bool
balanced (Leaf' y) = True
balanced (Node' l r) = balanced l && balanced r && (countLeaves l == countLeaves r)

-- c8e4
--- Define a function balance :: [a] -> Tree a that converts a
--- non-empty list into a balanced tree.
balance :: [a] -> Tree' a
balance [x] = (Leaf' x)
balance xs = Node' (balance l) (balance r)
  where
    (l, r) = halve xs

-- c8e5
--- Given the type declaration define a higher-order function such
--- that folde f g replaces each Val constructor in an expression by
--- the function f, and each Add constructor by the function g.
data Expr = Val Int | Add Expr Expr
  deriving Show

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- c8e6
--- Using folde, define a function eval :: Expr -> Int that evaluates
--- an expression to an integer value, and a function size :: Expr ->
--- Int that calculates the number of values in an expression.
eval :: Expr -> Int
eval exp = folde id (+) exp

size :: Expr -> Int
size exp = folde (const 1) (+) exp

-- c8e7
--- Complete the following instance declarations:
{-
instance Eq a => Eq (Maybe a) where
  Nothing == Nothing = True
  Just x == Just y = x == y
  _ == _ = False

instance Eq a => Eq [a] where
  [] == [] = True
  [] == _ = False
  _ == [] = False
  xs == ys = all (\(x, y) -> x == y) (zip xs ys)
  _ == _ = False
-}

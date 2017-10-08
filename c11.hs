-- c12e1
--- Define an instance of the Functor class for the following type of
--- binary trees that have data in their nodes:
data Tree a = Leaf | Node (Tree a) a (Tree a)
            deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf = Leaf
  fmap f (Node l x r) = (Node (fmap f l) (f x) (fmap f r))

-- c12e2
--- Complete the following instance declaration to make the
--- partially-applied function type (a ->) into a functor:
newtype P a b =  P (a -> b)

instance Functor (P a) where
  -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
  fmap g (P a)= P (g . a)

-- c12e3
--- Define an instance of the Applicative class for the type (a
--- ->). If you are familiar with combinatory logic, you might
--- recognise pure and <*> for this type as being the well-known K and
--- S combinators.
instance Applicative (P a) where
  -- pure :: b -> (a -> b)
  pure x = P (\_ -> x)
  -- <*> :: (b -> c) -> (a -> b) -> (a -> c)
  (P f) <*> (P g) = P (\x -> (f x) (g x))

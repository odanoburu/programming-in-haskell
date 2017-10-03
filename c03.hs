-- c3e2
--- write definition with specified type
add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply fn x = fn x

-- c3e3
--- what are the types of the following functions?
swap (x, y) = (y, x)
swap :: (a, b) -> (b, a)

pair x y = (x, y)
pair :: a -> b -> (a, b)

double x = x*2
double :: Num a => a -> a

palindrome xs = reverse xs == xs
palindrome :: Eq a => [a] -> Bool

twice f x = f (f x)
twice :: (a -> a) -> a -> a
-- is t special? (seems not to be, but dunno.)
--twice :: (t -> t) -> t -> t

-- c3e5
--- why is it no feasible in general for function types to be
--- instances of the Eq class? When is it feasible?
---- because if functions of the same type are equal when they always
---- return equal results for equal arguments, one must account for
---- infinite possibilities, unless the number of argument
---- combinations is finite. (or maybe it always returns the same
---- value.) but what about non-deterministic functions? this
---- definition would not work for them, in a way.

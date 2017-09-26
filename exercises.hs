-- c1e3
--- define a product function
prod :: Num a => [a] -> a
prod [] = 1
prod (x:xs) = x * product xs

-- c1e4
--- how to make a rever quicksort?
---- original:
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]
---- reverse:
rqsort :: Ord a => [a] -> [a]
rqsort [] = []
rqsort (x:xs) = rqsort larger ++ [x] ++ rqsort smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

-- c1e5
--- what happens when we drop the = in <= in qsort?
---- ans: repeated items are collapsed into one
uqsort :: Ord a => [a] -> [a]
uqsort [] = []
uqsort (x:xs) = uqsort smaller ++ [x] ++ uqsort larger
  where
    smaller = [a | a <- xs, a < x]
    larger = [b | b <- xs, b > x]

-- c2e4
--- define library function last in two different ways
---- obs: lib last raises exception when last [] -- why?
mylast1 :: [a] -> a
mylast1 xs = head (reverse xs)

mylast2 :: [a] -> a
mylast2 xs = xs !! (length xs -1)

mylast3 :: [a] -> a
mylast3 xs = head (drop (length xs -1) xs)

-- c2e5
--- define library function init in two different ways
myinit1 :: [a] -> [a]
myinit1 xs = take (length xs -1) xs

myinit2 :: [a] -> [a]
myinit2 xs = reverse (tail (reverse xs))

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

-- c4e1
--- define a function halve that splits an even-lengthed list into two
--- halves.
halve :: [a] -> ([a], [a])
halve xs = splitAt m xs
  where
    m = (length xs) `div` 2

-- c4e2
--- define a function thir that returns the third element in a list
--- that contains at least this many elements using:
third :: [a] -> a
---- a. head and tail
third xs = head (tail (tail xs))
---- b. list indexing
--third xs = xs !! 2
---- c. pattern matching
--third (_:_:x:_) = x

-- c4e3
--- define safetail (which is like rest in CL), using:
safetail :: [a] -> [a]
---- a. conditional expression
safetail xs = if null xs then [] else tail xs
---- b. guarded equations
--safetail xs | null xs = []
--            | otherwise = tail xs
---- c. pattern matching
--safetail [] = []
--safetail xs = tail xs

-- c4e4
--- show how disjunction operator || can be defined in four different
--- ways using pattern matching.
disj :: Bool -> Bool -> Bool
---- a.
disj True True = True
disj True False = True
disj False True = True
disj False False = False
---- b.
--disj True _ = True
--disj False a = a
---- c.
--disj False False = False
--disj _ _ = True
---- d.
--disj a b | a == b = a
--         | otherwise = True

conj :: Bool -> Bool -> Bool
-- c4e5
--- formalize && as in definition using conditional expressions:
--conj a b = if a == False then False else if b == True then True else False

-- c4e6
--- formalize && as in definition using conditional expressions:
conj a b = if a == True then b else False

-- c4e7
--- formalize mult x y z = z*y*z using lambda functions
mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x*y*z

-- c4e8
--- luhn algorithm
luhnDouble :: Int -> Int
luhnDouble n | n < 5 = 2 * n
             | otherwise = 2 * n -9

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = ((luhnDouble w) + x + (luhnDouble y) + z) `mod` 10 == 0

-- c5e1
--- calculate the sum of the first one hundred integer squares using
--- list comprehension
---- sum [n^2 | n <- [1..100]]
sumNsquares :: Int -> Int
sumNsquares n = sum [m^2 | m <- [1..n]]

-- c5e2
--- suppose coordinate grid is given by the list of integer
--- pairs. produce its points.
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- c5e3
--- return coordinates not in the diagonal of a square grid
square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

-- c5e4
--- define replicate using list comprehension
myreplicate :: Int -> a -> [a]
myreplicate n a = [a | _ <- [1..n]]

-- c5e5
--- return list of all pythagorean triples whose components do not
--- exceedd a given limit
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- c5e6
--- return list of all perfect numbers up to a limit
factors :: Int -> [Int]
factors n = [m | m <- [1..n], mod n m == 0]

perfects :: Int -> [Int]
perfects n = [m | m <- [1..n], m == (sum (factors m) - m)]

-- c5e7
--- show how one comprehension of type specified can be substituted by
--- two of them
pairs :: [a] -> [b] -> [(a,b)]
--pairs xs ys = [(x,y) | x <- xs, y <- ys]
--pairs xs ys = [p | x <- xs, p <- [(x,y) | y <- ys]]
pairs xs ys = concat [[(x,y) | x <- xs] | y <- ys]
--- why does the last one seem faster? I'd've guessed it should be
--- slower...

-- c5e8
--- redefine function positions using the function find
find :: Eq a => a -> [(a,b)] -> [b]
find k ps = [v | (k', v) <- ps, k' == k]

positions :: Eq a => a -> [a] -> [Int]
--positions x xs = [ix | (x', ix) <- zip xs [0..], x' == x]
positions x xs = find x (zip xs [0..])

-- c5e9
--- define scalar product using list comprehension
scalarproduct :: Num a => [a] -> [a] -> a
scalarproduct xs ys = sum [x * y | (x,y) <- zip xs ys]

-- c6e1
--- How does the recursive version of the factorial function behave if
--- applied to a negative argument, such as (-1)? Modify the
--- definition to prohibit negative arguments by adding a guard to the
--- recursive case.
---- it loops indefinitely.
factorial :: Int -> Int
factorial 0 = 1
factorial n | n > 0 = n * factorial (n-1)

-- c6e2
--- Define a recursive function sumdown :: Int -> Int that returns the
--- sum of the non-negative integers from a given value down to
--- zero. For example, sumdown 3 should return the result 3+2+1+0 = 6.
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n | n > 0 = n + sumdown (n-1)

-- c6e3
--- Define the exponentiation operator ^ for non-negative integers
--- using the same pattern of recursion as the multiplication operator
--- *, and show how the expression 2 ^ 3 is evaluated using your
--- definition.
myexp :: Int -> Int -> Int
myexp 0 _ = 0
myexp _ 0 = 1
myexp n k = n * (myexp n (k-1))
---- myexp 2 3 -- apply myexp
---- 2 * myexp 2 2 -- apply myexp
---- 2 * 2 * myexp 2 1 -- apply myexp
---- 2 * 2 * 2 myexp 2 0 -- apply myexp
---- 2 * 2 * 2 * 1 -- apply (*)
---- 8

-- c6e4
--- Define a recursive function euclid :: Int -> Int -> Int that
--- implements Euclid’s algorithm for calculating the greatest common
--- divisor of two non-negative integers: if the two numbers are
--- equal, this number is the result; otherwise, the smaller number is
--- subtracted from the larger, and the same process is then
--- repeated.
euclid :: Int -> Int -> Int
euclid m n | m > n = euclid (m - n) n
           | m == n = n
           | otherwise = euclid m (n - m)

-- c6e5
--- Using the recursive definitions given in this chapter, show how
--- length [1,2,3], drop 3 [1,2,3,4,5], and init [1,2,3] are
--- evaluated.
---- init [1,2,3] -- apply init
---- 1:init[2,3] -- apply init
---- 1:2:init[3] -- apply init
---- 1:2:[] -- list notation
---- [1,2]

-- c6e6
--- Without looking at the definitions from the standard prelude,
--- define the following library functions on lists using recursion.
---- a. and
myand :: [Bool] -> Bool
myand [b] = b
myand (x:xt) | x == False = False
             | otherwise = myand xt

---- b. concat
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xt) = x ++ concat xt

---- c. replicate
myrepl :: Int -> a -> [a]
myrepl 0 _ = []
myrepl n x = x:myrepl (n-1) x

---- d. (!!)
mynth :: [a] -> Int -> a
mynth (x:_) 0 = x
mynth (_:xt) n = mynth xt (n-1)

---- e. elem
myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem x (y:yt) | x == y = True
                | otherwise = myelem x yt

-- c6e7
--- Define a recursive function merge :: Ord a => [a] -> [a] -> [a]
--- that merges two sorted lists to give a single sorted list.
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs@(x:xt) ys@(y:yt) | x >= y = y : merge xs yt
                          | otherwise = x : merge xt ys

-- c6e8
--- Using merge, define a function msort :: Ord a => [a] -> [a] that
--- implements merge sort, in which the empty list and singleton lists
--- are already sorted, and any other list is sorted by merging
--- together the two lists that result from sorting the two halves of
--- the list separately.
---- halve defined in c4e1
msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort xs = merge (msort (fst halves)) (msort (snd halves))
  where
    halves = halve xs

-- c6e9
--- construct the library functions that:
---- a. calculate the sum of a list of numbers;
mysum :: Num a => [a] -> a
mysum [] = 0
mysum (x:xt) = x + mysum xt

---- b. take a given number of elements from the start of a list;
----- my take on take is
mytake :: Int -> [a] -> [a]
mytake 0 _ = []
mytake n [] = []
mytake n (x:xt) = x : mytake (n-1) xt

---- c. select the last element of a non-empty list
mylast :: [a] -> a
mylast [x] = x
mylast (_:xt) = mylast xt

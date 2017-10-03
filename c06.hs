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
exp' :: Int -> Int -> Int
exp' 0 _ = 0
exp' _ 0 = 1
exp' n k = n * (exp' n (k-1))
---- exp' 2 3 -- apply exp'
---- 2 * exp' 2 2 -- apply exp'
---- 2 * 2 * exp' 2 1 -- apply exp'
---- 2 * 2 * 2 exp' 2 0 -- apply exp'
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
and' :: [Bool] -> Bool
and' [b] = b
and' (x:xt) | x == False = False
             | otherwise = and' xt

---- b. concat
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xt) = x ++ concat xt

---- c. replicate
repl' :: Int -> a -> [a]
repl' 0 _ = []
repl' n x = x:repl' (n-1) x

---- d. (!!)
nth' :: [a] -> Int -> a
nth' (x:_) 0 = x
nth' (_:xt) n = nth' xt (n-1)

---- e. elem
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:yt) | x == y = True
                | otherwise = elem' x yt

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
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xt) = x + sum' xt

---- b. take a given number of elements from the start of a list;
----- my take on take is
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n [] = []
take' n (x:xt) = x : take' (n-1) xt

---- c. select the last element of a non-empty list
last' :: [a] -> a
last' [x] = x
last' (_:xt) = last' xt

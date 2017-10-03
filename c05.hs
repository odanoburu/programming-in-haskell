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
replicate' :: Int -> a -> [a]
replicate' n a = [a | _ <- [1..n]]

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

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

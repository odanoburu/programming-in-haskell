-- c2e4
--- define library function last in two different ways
---- obs: lib last raises exception when last [] -- why?
last1' :: [a] -> a
last1' xs = head (reverse xs)

last2' :: [a] -> a
last2' xs = xs !! (length xs -1)

last3' :: [a] -> a
last3' xs = head (drop (length xs -1) xs)

-- c2e5
--- define library function init in two different ways
init1' :: [a] -> [a]
init1' xs = take (length xs -1) xs

init2' :: [a] -> [a]
init2' xs = reverse (tail (reverse xs))

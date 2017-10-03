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

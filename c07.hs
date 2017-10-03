-- c7e1
--- Show how the list comprehension [f x | x <- xs, p x] can be
--- re-expressed using the higher-order functions map and filter.
---- map f (filter p xs)

-- c7e2
--- define the following higher-order library functions on lists:
---- a. Decide if all elements of a list satisfy a predicate:
all' :: (a -> Bool) -> [a] -> Bool
--all' f = and . map f
--all' f = foldr (&&) True . map f
all' f = foldr (\x y -> (f x) && y) True

---- b. Decide if any element of a list satisfies a predicate:
any' :: (a -> Bool) -> [a] -> Bool
--any' f = or . map f
--any' f = foldr (\x y -> (f x) || y) False
any' f = foldl (\x y -> x || f y) False

---- c. Select elements from a list while they satisfy a predicate:
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' pf (x:xt) | pf x = x:takeWhile' pf xt
                     | otherwise = []

---- d. Remove elements from a list while they satisfy a predicate:
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' pf xs@(x:xt) | pf x = dropWhile' pf xt
                        | otherwise = xs

-- c7e3
--- Redefine the functions map f and filter p using foldr.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xt -> (f x):xt) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' pf = foldr (\x xt -> if pf x then  x:xt else xt) []

-- c7e4
--- Using foldl, define a function dec2int :: [Int] -> Int that
--- converts a decimal number into an integer.
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

-- c7e5
--- Define the higher-order library function curry that converts a
--- function on pairs into a curried function, and, conversely, the
--- function uncurry that converts a curried function with two
--- arguments into a function on pairs.
curry' :: ((a,b) -> c) -> a -> b -> c
--curry' f = (\x -> (\y -> f (x,y)))
curry' f = \x y -> f (x,y)

uncurry' :: (a -> b -> c) -> (a,b) -> c
--uncurry' f = (\p -> f (fst p) (snd p))
uncurry' f = \(x,y) -> f x y

-- c7e6
--- Redefine the functions chop8, map f and iterate f using unfold.
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold null (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f

-- c7e7
--- Modify the binary string transmitter example to detect simple
--- transmission errors using the concept of parity bits.

-- Base conversion

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- Transmission

parityBit :: [Bit] -> Bit
parityBit = (`mod` 2) . sum

addParity :: [Bit] -> [Bit]
addParity xs = parityBit xs : xs

encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 []   = []
chop8 bits = take 9 bits : chop8 (drop 9 bits)

checkParity :: [Bit] -> [Bit]
checkParity (b:bt) | b == parityBit bt = bt
                   | otherwise = error "parity error"

decode :: [Bit] -> String
decode = map (chr . bin2int . checkParity) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- c7e8
--- Test your new string transmitter program using a faulty
--- communication channel that forgets the first bit, which can be
--- modelled using the tail function on lists of bits.
-- decode (tail (encode "opa"))
-- decode (channel (encode "opa"))

-- c7e9
--- Define a function altMap that alternately applies its two argument
--- functions to successive elements in a list, in turn about order.
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xt) = (f x) : altMap g f xt

-- c7e10
--- Using altMap, define a function luhn :: [Int] -> Bool that
--- implements the Luhn algorithm from the exercises in chapter 4 for
--- bank card numbers of any length. Test your new function using your
--- own bank card.
doubleWrap9 :: Int -> Int
doubleWrap9 n = if n > 4 then ((double n) - 9) else double n

luhn' :: [Int] -> Bool
luhn' = (== 0) . (`mod` 10) . sum . (altMap id doubleWrap9) . reverse

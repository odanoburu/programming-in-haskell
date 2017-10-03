import Data.Char

-- c10e1
--- Redefine putStr :: String -> IO () using a list comprehension and
--- the library function sequence_ :: [IO a] -> IO ().
putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <- xs]

-- c10e2
--- Using recursion, define a version of putBoard :: Board -> IO ()
--- that displays nim boards of any size, rather than being specific
--- to boards with just five rows of stars.
type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard' :: Int -> Board -> IO ()
putBoard' _ [] = return ()
putBoard' n (x:xt) = do putRow n x
                        putBoard' (n+1) xt

putBoard :: Board -> IO ()
putBoard = putBoard' 1

-- c10e3
--- In a similar manner to the first exercise, redefine the generalised
--- version of putBoard using a list comprehension and sequence_.
putBoard'' :: Board -> IO ()
putBoard'' xs = sequence_ [putRow n x | (n, x) <- zip [1..] xs]

-- c10e4
--- Define an action adder :: IO () that reads a given number of
--- integers from the keyboard, one per line, and displays their sum.
newline :: IO ()
newline = putChar '\n'

adder' :: Int -> Int -> IO Int
adder' 0 t = t
adder' n s = do k <- getLine
                adder' (n-1) (s + (read k :: Int))

{-adder :: IO ()
adder = do putStr "how many numbers?"
           read-}

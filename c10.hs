import Data.Char
import System.IO

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
readNum :: IO Int
readNum = do strn <- getLine
             let n = read strn :: Int
             return n

adder' :: Int -> IO Int
adder' remaining = do
  if (remaining /= 0)
    then do
      number <- readNum
      restsum <- adder' (remaining - 1)
      return (number + restsum)
    else do
      return 0

adder :: IO ()
adder = do putStrLn "how many numbers?"
           howmany <- readNum
           sum <- adder' howmany
           putStrLn ("The total is " ++ (show sum))
           return ()

-- c10e5
--- Redefine adder using the function sequence :: [IO a] -> IO [a]
--- that performs a list of actions and returns a list of the
--- resulting values.
adder'' :: IO ()
adder'' = do putStrLn "how many numbers?"
             howmany <- readNum
             numbers <- sequence $ replicate howmany readNum
             let total = sum numbers
             putStrLn ("The total is " ++ (show total))
             return ()


-- c10e6
--- Using getCh, define an action readLine :: IO String that behaves
--- in the same way as getLine, except that it also permits the delete
--- key to be used to remove characters. Hint: the delete character is
--- ’\DEL’, and the control character for moving the cursor back one
--- space is ’\b’.
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

readLine :: IO String
readLine = do c <- getCh
              case c of '\n' -> return []
                        '\DEL' -> do cs <- readLine
                                     return ('\b':cs)
                        _ -> do cs <- readLine
                                return (c:cs)

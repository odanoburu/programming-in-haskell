-- c9e1
--- Redefine the combinatorial function choices using a list
--- comprehension rather than using composition, concat and map.
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xt) = yts ++ map (x:) yts
  where yts = subs xt

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:yt) = (x:y:yt) : map (y:) (interleave x yt)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xt) = concat (map (interleave x) (perms xt))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

choices' :: [a] -> [[a]]
choices' xs = [y | ss <- subs xs, y <- perms ss]

-- c9e2
--- Define a recursive function isChoice :: Eq a => [a] -> [a] -> Bool
--- that decides if one list is chosen from another, without using the
--- combinatorial functions perms and subs.
rmFrom :: Eq a => a -> [a] -> [a]
rmFrom _ [] = []
rmFrom y (x:xt) =
  if (y == x)
    then xt
    else x : rmFrom y xt

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice (x:xt) ys = elem x ys && isChoice xt (rmFrom x ys)

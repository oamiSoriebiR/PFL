myandCompreension :: [Bool] -> Bool
myandCompreension xs = null ([x | x <- xs, not x])

myandRecursion :: [Bool] -> Bool
myandRecursion [] = True
myandRecursion (x:xs) = x && myandRecursion xs

myor :: [Bool] -> Bool
myor [] = False
myor (x:xs) = x || myor xs

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

myreplicateRecursive :: Int -> a -> [a]
myreplicateRecursive 0 _ = []
myreplicateRecursive n x = x : myreplicateRecursive (n-1) x

myIndexRec :: [a] -> Int -> a
myIndexRec [] _ = error "Index out of bounds"
myIndexRec (x:xs) 0 = x
myIndexRec (x:xs) n = myIndexRec xs (n-1)

elemRecursive :: Eq a => a -> [a] -> Bool
elemRecursive _ [] = False
elemRecursive y (x:xs) = (y == x) || elemRecursive y xs

leastDiv :: Int -> Int
leastDiv x
    | x `mod` 2 == 0 = 2
    | otherwise      = leastDiv' x 3
    where
        leastDiv' n d
            | d * d > n      = n
            | n `mod` d == 0 = d
            | otherwise      = leastDiv' n (d + 2)

isPrimeFast :: Int -> Bool
isPrimeFast x
    | x <= 1    = False
    | otherwise = leastDiv x == x

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs)
    | x `elem` xs = x : nub (filter (/= x) xs)
    | otherwise   = x : nub xs


intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs

insertRecursive :: Ord a => a -> [a] -> [a]

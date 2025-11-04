import Distribution.Simple.Utils (safeTail)
-- Using conditional expressions
classify :: Int -> String
classify n =
    if n <= 9 then "failed"
    else if n <= 12 then "passed"
    else if n <= 15 then "good"
    else if n <= 18 then "very good"
    else if n <= 20 then "excellent"
    else "invalid grade"

-- Using guards
classify' :: Int -> String
classify' n
    | n <= 9    = "failed"
    | n <= 12   = "passed"
    | n <= 15   = "good"
    | n <= 18   = "very good"
    | n <= 20   = "excellent"
    | otherwise = "invalid grade"


classifyBMI' :: Float -> Float -> String
classifyBMI' weight height =
    let bmi = weight / (height * height)
    in if bmi < 18.5 then "underweight"
       else if bmi < 25 then "normal weight"
       else if bmi < 30 then "overweight"
       else "obese"


classifyBMI :: Float -> Float -> String
classifyBMI weight height
    | bmi < 18.5 = "underweight"
    | bmi < 25   = "normal weight"
    | bmi < 30   = "overweight"
    | otherwise  = "obese"
    where bmi = weight / (height * height)

max3 :: Ord a => a -> a -> a -> a
max3 x y z
    | x >= y && x >= z = x
    | y >= x && y >= z = y
    | otherwise        = z

min3 :: Ord a => a -> a -> a -> a
min3 x y z
    | x <= y && x <= z = x
    | y <= x && y <= z = y
    | otherwise        = z

max3' :: Ord a => a -> a -> a -> a
max3' x y z = max x (max y z)

min3' :: Ord a => a -> a -> a -> a
min3' x y z = min x (min y z)


xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _        = False


-- Using conditional expressions
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

-- Using guards
safetail' :: [a] -> [a]
safetail' xs
    | null xs   = []
    | otherwise = tail xs

-- Using patterns
safetail'' :: [a] -> [a]
safetail'' []     = []
safetail'' (_:xs) = xs

short :: [a] -> Bool
short n = length n < 3

short' :: [a] -> Bool
short' [] = True
short' [_] = True
short' [_, _] = True
short' _ = False


-- REDUCE COMPARISONS
median :: Ord a => a -> a -> a -> a
median x y z
    | (x <= y && y <= z) || (z <= y && y <= x) = y
    | (y <= x && x <= z) || (z <= x && x <= y) = x
    | otherwise = z

median' :: (Ord a, Num a) => a -> a -> a -> a
median' x y z = x + y + z - min3' x y z - max3' x y z


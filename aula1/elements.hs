second :: [a] -> a
second xs = head (tail xs)

last :: [a] -> a
last xs = head (reverse xs)

init :: [a] -> [a]
init xs = reverse (tail (reverse xs))

middle :: [a] -> a
middle xs = head (drop (length xs `div` 2) xs)

checkPalindrome :: Eq a => [a] -> Bool
checkPalindrome xs = xs == reverse xs

checkTriangle :: Float -> Float -> Float -> Bool
checkTriangle a b c = a + b > c && a + c > b && b + c > a

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = sqrt (s * (s - a) * (s - b) * (s - c))
    where s = (a + b + c) / 2
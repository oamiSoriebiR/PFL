import Test.QuickCheck
import Data.Ratio (denominator)
import Data.Char
calcPi1 :: Int -> Double
calcPi1 n = sum $ take n $ zipWith (/) (cycle [4.0, -4.0]) [1.0,3.0..]

calcPi2 :: Int -> Double
calcPi2 n = 3 + sum (take n $ zipWith (/) (cycle [4.0, -4.0]) denominators)
  where
    denominators = [k * (k+1) * (k+2) | k <- [2,4..]]


twinPrimes :: [(Int, Int)]
twinPrimes = [(p, p+2) | p <- primes, isPrime (p+2)]
  where
    primes = sieve [2..]
    sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x /= 0]
    isPrime n = null [x | x <- takeWhile (\x -> x*x <= n) primes, n `mod` x == 0]

gen235 :: Int -> [Int]
gen235 n = [2^i * 3^j * 5^k | i <- [0..n], j <- [0..n-i], let k = n - i - j]


hamming :: [Int]
hamming = 1 : merge (map (*2) hamming) (merge (map (*3) hamming) (map (*5) hamming))
  where
    merge (x:xs) (y:ys)
      | x < y     = x : merge xs (y:ys)
      | x > y     = y : merge (x:xs) ys
      | otherwise = x : merge xs ys


rot13 :: String -> String
rot13 = map rotChar
  where
    rotChar c
      | isAsciiLower c = chr $ (ord c - ord 'a' + 13) `mod` 26 + ord 'a'
      | isAsciiUpper c = chr $ (ord c - ord 'A' + 13) `mod` 26 + ord 'A'
      | otherwise = c

    ord = fromEnum
    chr = toEnum


main :: IO ()
main = do
    putStr "Give me your secret message: "
    message <- getLine
    putStrLn ("Encrypted message: " ++ rot13 message ++ ", shhhhhhhhh!") 

prop_roundTrip :: String -> Property
prop_roundTrip s = rot13 (rot13 s) === s

type AWord = String
type Line = [AWord]
type Paragraph = [Line]

fillWords :: Int -> Line -> Paragraph
fillWords maxWidth = fillLines
  where
    fillLines [] = []
    fillLines ws = let (line, rest) = fillLine ws in line : fillLines rest

    fillLine [] = ([], [])
    fillLine (w:ws)
      | length w > maxWidth = ([w], ws) 
      | otherwise = let (line, rest) = fillLine' ws (length w) [w]
                    in (line, rest)

    fillLine' [] _ acc = (reverse acc, [])
    fillLine' (w:ws) currentLen acc
      | currentLen + 1 + length w <= maxWidth = fillLine' ws (currentLen + 1 + length w) (w:acc)
      | otherwise = (reverse acc, w:ws)



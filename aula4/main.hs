import Data.Char
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
    putStr "Give me your secret message: \n"
    message <- getLine
    putStrLn ("Encrypted message: " ++ rot13 message ++ ", shhhhhhhhh!\n") 
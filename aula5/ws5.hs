import Test.QuickCheck.Text (Str)
data List a = Empty | Cons a (List a)



-- fromList (toList xs) == xs
-- these functions are inverse of one another

toList :: [a] -> List a 
toList [] = Empty
toList (x:xs) = Cons x (toList xs)

fromList :: List a -> [a]
fromList Empty = []
fromList (Cons x xs) = x : fromList xs

data Face = A | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | J | Q | K
  deriving (Show, Eq, Enum)

data Suit = C | D | H | S
  deriving  (Show, Eq, Enum)

data Card = Card { f :: Face, s :: Suit }
 deriving (Show)

allCards :: [Card]
allCards = [Card f s | f <- [A .. K], s <- [C .. S]]




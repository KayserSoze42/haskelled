module Mo3 (hanoi, moveCount, testDrop, truer, falser, ander) where

type Peg = [Char]
type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ hanoi 1 a b c ++ hanoi (n - 1) c b a

moveCount :: [Move] -> Int
moveCount []     = 0
moveCount (x:[]) = 1
moveCount (x:zs) = 1 + (moveCount zs)

testDrop :: Int -> [Char] -> [Char]
testDrop n xs
  |  n <= 0 || (xs == []) = []
  |  otherwise            = testDrop (n - 1) (tail xs)

truer :: Bool -> Bool -> Bool
truer x y = x

falser :: Bool -> Bool -> Bool
falser x y = y

ander :: Bool -> Bool -> Bool
ander x y
  | x == False = False
  | y == False = False
  | otherwise  = True

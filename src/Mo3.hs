module Mo3 (hanoi, moveCount) where

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

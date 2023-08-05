module Mo1 (hailstoneSeq, is6or9, getOpinion, intListLength) where

hailstone :: Integer -> Integer
hailstone n 
  |  n `mod` 2 == 0 = n `div` 2
  | otherwise       = 3 * n + 1

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

is6or9 :: Integer -> Bool
is6or9 6 = True
is6or9 9 = True
is6or9 x = False

getOpinion :: Integer -> [Char]
getOpinion x
  |  (is6or9 x) == True = "n1"
  |  x == 42            = "Well it means something, alright"
  |  x == 69            = "Nice.."
  |  x == 420           = "Pretty nice.."
  |  x == 42069         = "Pretty.. pretty.. pretty nicee"
  |  otherwise          = "Meh.."

intListLength :: [Integer] -> Int
intListLength []     = 0
intListLength (x:xs) = 1 + intListLength xs

module Mo9 
         ( isFunny
	 , fizzBuzz
	 , inCert
	 , inSorty
	 , listToChars
	 ) where

isFunny :: Int -> Bool
isFunny n
  | firstDigit == 6 && lastDigit == 9 = True
  | otherwise                         = False
  where
        firstDigit = n `div` 10
	lastDigit  = n `mod` 10

fizzBuzz :: Int -> Int -> [Int] -> [[Char]]
fizzBuzz fizz buzz numbers = map (fizzbuzzable) numbers 
                             where
			           fizzbuzzable n 
				        | (fizzable n) && (buzzable n) == True = "#" ++ show n ++ " - FizzBuzz - divisible by both " ++ show fizz ++ " and " ++ show buzz 
					| (fizzable n) == True                 = "#" ++ show n ++ " - Fizz - divisible by " ++ show fizz ++ " only"
					| (buzzable n) == True                 = "#" ++ show n ++ " - Buzz - divisible by " ++ show buzz ++ " only"
					| otherwise                            = "#" ++ show n ++ " - none - not divisible by neither " ++ show fizz ++ " nor " ++ show buzz
					where fizzable number = (number `mod` fizz) == 0
					      buzzable number = (number `mod` buzz) == 0


inCert :: Ord a => a -> [a] -> [a]
inCert x []               = [x]
inCert x (y:xs)
              | x < y     = x : y : xs
	      | otherwise = y : (inCert x xs)

inSorty :: Ord a => [a] -> [a]
inSorty []     = []
inSorty (x:xs) = inCert x (inSorty xs)

listToChars :: [[Char]] -> [Char]
listToChars []     = []
listToChars (x:[]) = (show x) ++ (listToChars [])
listToChars (x:xs) = (show x) ++ (listToChars xs)


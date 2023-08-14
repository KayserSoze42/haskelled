module Mo2 
         ( printWM
	 , toReversedList
	 , doubleEverySecond
	 , sumItUp
	 , isValid
	 , isValidCreditCard
	 , CreditCard (..)
	 , CCID
	 , CCN
	 ) where

type CCID = Int
type CCN  = Int

data CreditCard = CreditCard {
                               ccID :: CCID
                             , ccN  :: CCN
                             } deriving (Show, Eq)
printWM :: [Char]
printWM = "Mo2"

doubled :: Int -> Int
doubled n = 2 * n

toReversedList :: Integral x => x -> [x]
toReversedList 0 = []
toReversedList x = (x `mod` 10) : (toReversedList (x `div` 10))

doubleEverySecond :: [Int] -> [Int]
doubleEverySecond []       = []
doubleEverySecond (x:[])   = [x]
doubleEverySecond (x:y:zs) = x : (doubled y) : (doubleEverySecond zs)

sumItUp :: [Int] -> Int
sumItUp []      = 0
sumItUp (x:[])
  | x >= 10     = sumItUp (toReversedList x)
  | otherwise   = x
sumItUp (x:ys)
  | x >= 10     = (sumItUp (toReversedList x)) + (sumItUp ys)
  | otherwise   = x + (sumItUp ys)

isValid :: Int -> Bool
isValid n
  | n `mod` 10 == 0 = True
  | otherwise       = False

isValidCreditCard :: Int -> Bool
isValidCreditCard x = isValid (sumItUp (doubleEverySecond (toReversedList (x))))

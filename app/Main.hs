module Main where

import qualified Mo1 (hailstoneSeq, is6or9, getOpinion, intListLength, intToSum)
import qualified Mo2 (printWM, toReversedList, doubleEverySecond, sumItUp, isValid, isValidCreditCard, CreditCard (..), CCID, CCN)
import qualified Mo3 (hanoi, moveCount, testDrop, truer, falser, ander)
import qualified Mo4 (javaRecipe, Recipe (..))
import qualified Mo5 (Complex (..), Tree (..))

list :: [Int]
list = [4, 8, 15, 16, 23, 42]

list2 :: [[Char]]
list2 = Mo4.ingredients Mo4.javaRecipe

list3 :: [Int]
list3 = [(a) | a <- [1..1000]]

creditCard1 :: Mo2.CreditCard
creditCard1 = Mo2.CreditCard 1 4929085012928546

creditCard2 :: Mo2.CreditCard
creditCard2 = Mo2.CreditCard 2 4716893498012975

creditCard3 :: Mo2.CreditCard
creditCard3 = Mo2.CreditCard 3 345078769084195 -- apex amex

tree1 :: Mo5.Tree String
tree1 = Mo5.Node "toor" (Just (Mo5.Node "left" (Nothing) (Nothing)))
                        (Just (Mo5.Node "right" (Nothing) (Nothing)))

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

main :: IO ()
main = do
 
  let fizzbuzzedList = fizzBuzz 6 9 list3 

  mapM_ putStrLn fizzbuzzedList

  -- mapM_ (appendFile "fizzbuzz.txt") (map (++ "\n") fizzbuzzedList)

  putStrLn ("Done!")

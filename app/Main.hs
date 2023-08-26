module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

import qualified Mo1 
                   ( hailstoneSeq
		   , is6or9
		   , getOpinion
		   , intListLength
		   , intToSum
		   )

import qualified Mo2 
                   ( printWM
		   , toReversedList
		   , doubleEverySecond
		   , sumItUp, isValid
		   , isValidCreditCard
		   , CreditCard (..)
		   , CCID
		   , CCN
		   )

import qualified Mo3 
                   ( hanoi
		   , moveCount
		   , testDrop
		   , truer
		   , falser
		   , ander
		   )

import qualified Mo4 
                   ( javaRecipe
		   , Recipe (..)
		   )

import qualified Mo5 
                   ( Complex (..)
		   , Tree (..)
		   )

import qualified Mo6 
                   ( TimeStamp
		   , MessageType (..)
		   , LogMessage (..)
		   , MessageBinTree (..)
		   , parseSingle
		   , parseAll
		   , insert
		   , buildTree
		   , inDisOrder
		   , inReOrder
		   , inPostOrder
		   )

import qualified Mo7
                   ( absDeez
		   , getStampd
		   , Vic (..)
		   , getDeez
		   --
		   )

import qualified Mo8
                   ( bubblePleez
		   , encod3
		   , decod3
		   )

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

beanie1 :: Mo6.MessageBinTree
beanie1 = Mo6.Empty

beanie2 :: Mo6.MessageBinTree
beanie2 = Mo6.Leaf (Mo6.LogMessage Mo6.Info 69 "69-fo-sho")

listToChars :: [[Char]] -> [Char]
listToChars []     = []
listToChars (x:[]) = (show x) ++ (listToChars [])
listToChars (x:xs) = (show x) ++ (listToChars xs)

main :: IO ()
main = do
 
  executionStart <- getCurrentTime

  -- dumb stuff go here --

  let message = "That's funny, I don't feel corrupt.."

  let messag2 = "In fact, I feel pretty fine, just pretty fine ..."

  putStrLn (show message)

  putStrLn ("---")

  putStrLn (show messag2)

  let corrupted = Mo8.encod3 6 message

  let corrupte2 = Mo8.encod3 9 messag2

  putStrLn ("*************")

  putStrLn (show corrupted)

  putStrLn ("---")

  putStrLn (show corrupte2)
  
  disorderExecutionEnd <- getCurrentTime

  putStrLn ("Total, run time: " ++ (show (diffUTCTime disorderExecutionEnd executionStart)))

  putStrLn ("----------------------")
  putStrLn ("job (somewhat) done!")

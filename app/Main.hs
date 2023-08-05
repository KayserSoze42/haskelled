module Main where

import qualified Mo1 (hailstoneSeq, is6or9, getOpinion, intListLength, intToSum)
import qualified Mo2 (printWM, toReversedList, doubleEverySecond, sumItUp, isValid, isValidCreditCard)
import qualified Mo3 (hanoi, moveCount)

list :: [Int]
list = [4, 8, 15, 16, 23, 42]

main :: IO ()
main = do

  putStrLn ("Enter a number: ")

  number <- readLn :: IO Int

  putStrLn ("-----------------------------------")
  
  putStrLn ("Is given number 6 or 9? ")

  putStrLn (show (Mo1.is6or9 number))

  putStrLn ("-----------------------------------")
  
  putStrLn ("Hailstone: ")
  
  putStrLn (show (Mo1.hailstoneSeq number))

  putStrLn ("-----------------------------------")
  
  putStrLn ("Opinion: ")

  putStrLn ((Mo1.getOpinion number))

  putStrLn ("-----------------------------------")
 
  putStrLn ("List before: ")

  putStrLn (show (list))

  putStrLn ("Length: ")

  putStrLn (show (Mo1.intListLength list))

  putStrLn ("Two sum it up: ")

  putStrLn (show (Mo1.intToSum list))

  putStrLn ("-----------------------------------")
  
  let afterList = number:list

  putStrLn ("List after: ")

  putStrLn (show (afterList))

  putStrLn ("After length: ")

  putStrLn (show (Mo1.intListLength afterList))

  putStrLn ("Two sum it up 2: ")

  putStrLn (show (Mo1.intToSum afterList))

  putStrLn ("-----------------------------------")
  
  putStrLn ("Working Module: " ++ Mo2.printWM)

  putStrLn ("Enter a number... again: ")

  number2 <- readLn :: IO Int

  putStrLn ("Your number: " ++ show (number2))

  putStrLn ("Your number... but in a reversed list: ")

  putStrLn (show (Mo2.toReversedList number2))

  putStrLn ("Your number... but in a reversed list with every second number doubled:")

  putStrLn (show (Mo2.doubleEverySecond (Mo2.toReversedList number2)))

  putStrLn ("Your number... but in a reversed list with every second number doubled... and all digits summed: ")

  putStrLn (show (Mo2.sumItUp (Mo2.doubleEverySecond (Mo2.toReversedList number2))))

  putStrLn ("And.... is it a valid Credit Card number? ")

  putStrLn (show (Mo2.isValidCreditCard (number2)))

  let lastDigit = number2 `mod` 10

  let hanoiMoves = Mo3.hanoi lastDigit "first" "second" "third"

  let hanoiMovesCount = Mo3.moveCount hanoiMoves

  putStrLn ("To complete to so-called hanoi puzzle with 3 pegs and " ++ show (lastDigit) ++ " blocks it would take " ++ show (hanoiMovesCount) ++ " moves... or not..")

  putStrLn (show (hanoiMoves))


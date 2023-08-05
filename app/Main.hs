module Main where

import qualified Mo1 (hailstoneSeq, is6or9, getOpinion, intListLength, intToSum)
import qualified Mo2 (printWM, toReversedList, doubleEverySecond, sumItUp, isValid)

list :: [Integer]
list = [4, 8, 15, 16, 23, 42]

main :: IO ()
main = do

  print ("Enter a number: ")

  number <- readLn :: IO Integer

  print ("-----------------------------------")
  
  print ("Is given number 6 or 9? ")

  print (show (Mo1.is6or9 number))

  print ("-----------------------------------")
  
  print ("Hailstone: ")
  
  print (show (Mo1.hailstoneSeq number))

  print ("-----------------------------------")
  
  print ("Opinion: ")

  print ((Mo1.getOpinion number))

  print ("-----------------------------------")
 
  print ("List before: ")

  print (show (list))

  print ("Length: ")

  print (show (Mo1.intListLength list))

  print ("Two sum it up: ")

  print (show (Mo1.intToSum list))

  print ("-----------------------------------")
  
  let afterList = number:list

  print ("List after: ")

  print (show (afterList))

  print ("After length: ")

  print (show (Mo1.intListLength afterList))

  print ("Two sum it up 2: ")

  print (show (Mo1.intToSum afterList))

  print ("-----------------------------------")
  
  print ("Working Module: " ++ Mo2.printWM)

  print ("Enter a number... again: ")

  number2 <- readLn :: IO Integer

  print ("Your number: " ++ show (number2))

  print ("Your number... but in a reversed list: ")

  print (show (Mo2.toReversedList number2))

  print ("Your number... but in a reversed list with every second number doubled:")

  print (show (Mo2.doubleEverySecond (Mo2.toReversedList number2)))

  print ("Your number... but in a reversed list with every second number doubled... then summed: ")

  print (show (Mo2.sumItUp (Mo2.doubleEverySecond (Mo2.toReversedList number2))))

  print ("And.... is it a valid Credit Card number? ")

  print (Mo2.isValid (Mo2.sumItUp (Mo2.doubleEverySecond (Mo2.toReversedList number2))))


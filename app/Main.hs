module Main where

import qualified Mo1 (hailstoneSeq, is6or9, getOpinion, intListLength)

list :: [Integer]
list = [4, 8, 15, 16, 23, 42]

main :: IO ()
main = do

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

  print ("-----------------------------------")
  
  let afterList = number:list

  print ("List after: ")

  print (show (afterList))

  print ("After length: ")

  print (show (Mo1.intListLength afterList))

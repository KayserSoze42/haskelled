module Main where

import qualified Mo1 (hailstoneSeq, is6or9, getOpinion, intListLength, intToSum)
import qualified Mo2 (printWM, toReversedList, doubleEverySecond, sumItUp, isValid, isValidCreditCard)
import qualified Mo3 (hanoi, moveCount, testDrop, truer, falser, ander)
import qualified Mo4 (javaRecipe)

list :: [Int]
list = [4, 8, 15, 16, 23, 42]

main :: IO ()
main = do
  
  putStrLn ("Howdy there")

  putStrLn (show (Mo4.javaRecipe))

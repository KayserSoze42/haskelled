module Main where

import qualified Mo1 (hailstone, hailstoneSeq, is6or9, getOpinion)

main :: IO ()
main = do

  print ("Enter a number: ")

  input <- getLine
  let number = read input :: Integer
  
  print ("Is given number 6 or 9? " ++ show (Mo1.is6or9 number))

  print ("Hailstone: " ++ show (Mo1.hailstoneSeq number))

  print ("Opinion: " ++ (Mo1.getOpinion number))

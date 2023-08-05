module Main where

import qualified MyLib (addd, funkyFunc)

numberz :: [Int]
numberz = []

main :: IO ()
main = do

  print ("Numberz: " ++ show (numberz))

  print ("Enter a number: ")

  input <- getLine
  let numberzz = MyLib.addd (read input :: Int) numberz

  print ("Numberzz: " ++ show (numberzz))


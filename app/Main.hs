module Main where

import qualified Mo1 (hailstoneSeq, is6or9, getOpinion, intListLength, intToSum)
import qualified Mo2 (printWM, toReversedList, doubleEverySecond, sumItUp, isValid, isValidCreditCard, CreditCard (..), CCID, CCN)
import qualified Mo3 (hanoi, moveCount, testDrop, truer, falser, ander)
import qualified Mo4 (javaRecipe, Recipe (..))
import qualified Mo5 (Complex (..), Tree (..))

list :: [Int]
list = [4, 8, 15, 16, 23, 42]

creditCard1 :: Mo2.CreditCard
creditCard1 = Mo2.CreditCard 1 4929085012928546

creditCard2 :: Mo2.CreditCard
creditCard2 = Mo2.CreditCard 2 4716893498012975

creditCard3 :: Mo2.CreditCard
creditCard3 = Mo2.CreditCard 3 345078769084195 -- apex amex

tree1 :: Mo5.Tree String
tree1 = Mo5.Node "toor" (Just (Mo5.Node "left" (Nothing) (Nothing)))
                        (Just (Mo5.Node "right" (Nothing) (Nothing)))

main :: IO ()
main = do
  
  putStrLn (show (tree1))

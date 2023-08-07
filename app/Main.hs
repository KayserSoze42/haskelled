module Main where

import qualified Mo1 (hailstoneSeq, is6or9, getOpinion, intListLength, intToSum)
import qualified Mo2 (printWM, toReversedList, doubleEverySecond, sumItUp, isValid, isValidCreditCard, CreditCard (..), CCID, CCN)
import qualified Mo3 (hanoi, moveCount, testDrop, truer, falser, ander)
import qualified Mo4 (javaRecipe, Recipe (..))
import qualified Mo5 (Complex (..))

list :: [Int]
list = [4, 8, 15, 16, 23, 42]

creditCard1 :: Mo2.CreditCard
creditCard1 = Mo2.CreditCard 1 4929085012928546

creditCard2 :: Mo2.CreditCard
creditCard2 = Mo2.CreditCard 2 4716893498012975

creditCard3 :: Mo2.CreditCard
creditCard3 = Mo2.CreditCard 3 345078769084195 -- apex amex

main :: IO ()
main = do
  
  putStrLn ("Howdy there")

  putStrLn ("--------------------------------------------------------------------------")

  putStrLn ("To cook a silly thing called bytecode in " ++ show (Mo4.recipeName Mo4.javaRecipe) ++ ", You will need the following:")

  mapM_ putStrLn (Mo4.ingredients Mo4.javaRecipe)

  putStrLn ("--------------------------------------------------------------------------")

  putStrLn ("Performing Credit Card Luhn Checks: ")

  putStrLn ("--------------------------------------------------------------------------")

  putStrLn ("For Credit Card #" ++ show (Mo2.ccID creditCard1))

  putStrLn ("Valid card: " ++ show (Mo2.isValidCreditCard (Mo2.ccN creditCard1)))

  putStrLn ("--------------------------------------------------------------------------")

  putStrLn ("For Credit Card #" ++ show (Mo2.ccID creditCard2))

  putStrLn ("Valid card: " ++ show (Mo2.isValidCreditCard (Mo2.ccN creditCard2)))

  putStrLn ("--------------------------------------------------------------------------")

  putStrLn ("For Credit Card #" ++ show (Mo2.ccID creditCard3))

  putStrLn ("Valid card: " ++ show (Mo2.isValidCreditCard (Mo2.ccN creditCard3)))

  putStrLn ("--------------------------------------------------------------------------")

  putStrLn ("Enter your own ccid: ")

  ccidNumber <- readLn :: IO Mo2.CCID

  putStrLn ("Enter your own ccn: ")

  ccnNumber <- readLn :: IO Mo2.CCN

  let userCard = Mo2.CreditCard ccidNumber ccnNumber

  putStrLn ("Checking if valid: ")

  putStrLn (show (Mo2.isValidCreditCard (Mo2.ccN userCard)))

  putStrLn ("Done for noe..")

  let complexed = Mo5.Ende

  let complexeder = Mo5.Cmplx "Z" complexed

  let complexederer = Mo5.Cmplx "E" complexeder

  let complexedererer = Mo5.Cmplx "E" complexederer

  let complexederererer = Mo5.Cmplx "D" complexedererer

  putStrLn (show complexederererer)

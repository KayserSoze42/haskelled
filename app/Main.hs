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

import qualified Mo9
                   ( isFunny
		   , fizzBuzz
		   , inCert
		   , inSorty
		   , listToChars
		   )

main :: IO ()
main = do
 
  executionStart <- getCurrentTime

  -- dumb stuff go here --
 
  let aList = [6,9,4,2,23,1,33,7]

  putStrLn ("Ladies and gentlemen.. a list: " ++ show (aList))

  putStrLn ("-----")

  putStrLn ("And I present to you now... b list: " ++ show (Mo9.inSorty aList))

  putStrLn ("-----")

  putStrLn ("...that's all folks?")

  disorderExecutionEnd <- getCurrentTime

  putStrLn ("Total, run time: " ++ (show (diffUTCTime disorderExecutionEnd executionStart)))

  putStrLn ("----------------------")
  putStrLn ("job (somewhat) done!")

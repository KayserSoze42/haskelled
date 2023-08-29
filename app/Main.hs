module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

import qualified M00
                   ( BinaryTree (..)
		   , treleft
		   , treight
		   , trehigh
		   , tresize
		   , tremin
		   , tremax
		   , rootree
		   , treelem
		   , trinsert
		   , tremove
		   , listToTree
		   , listFrTree
		   , trelrot
		   , trerrot
		   )
		   
import qualified M01 
                   ( locVicinus
		   , breadth
		   , breadthVless
		   )

import qualified M02 
                   ( PoLiSt (..)
		   , fiLiSted
		   , leMapped
		   )

main :: IO ()
main = do
 
  executionStart <- getCurrentTime

  -- dumb stuff go here --

  putStrLn ("----------------------")
  putStrLn ("job (somewhat) done!")

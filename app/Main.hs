module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

import qualified M00
                   ( treleft
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
		   
main :: IO ()
main = do
 
  executionStart <- getCurrentTime

  -- dumb stuff go here --
 
  

  putStrLn ("----------------------")
  putStrLn ("job (somewhat) done!")

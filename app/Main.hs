module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

import qualified M03
                   ( JAAASON (..)  
		   )
main :: IO ()
main = do
 
  executionStart <- getCurrentTime

  -- dumb stuff go here --

  

  executionEnd <- getCurrentTime
  
  putStrLn ("----------------------")
  putStrLn ("job (somewhat) done!\nonly took: " ++ show (diffUTCTime executionEnd executionStart))

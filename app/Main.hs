module Main where

import Data.Time.Clock (getCurrentTime, diffUTCTime)

import qualified M03
                   ( JAAASON (..) 
		   )
main :: IO ()
main = do
 
  executionStart <- getCurrentTime

  -- dumb stuff go here --

  let jSON0 = M03.JSTR "if it compiles it works, right? .. RIGHT?!" :: M03.JAAASON
  let jSON1 = M03.JNMR 69 :: M03.JAAASON
  let jSON2 = M03.JBOO True :: M03.JAAASON

  let jSOF0 = M03.JOBB [("bigiftrue", jSON0)] :: M03.JAAASON
  let jSOF1 = M03.JOBB [("niceiftrue", jSON1)] :: M03.JAAASON
  let jSOF2 = M03.JOBB [("trueiftrue", jSON2)] :: M03.JAAASON

  let jARR0 = M03.JARR [jSON0, jSON1] :: M03.JAAASON
  let jARR1 = M03.JARR [jSON1, jSON2] :: M03.JAAASON
  let jARR2 = M03.JARR [jSON0, jSON2] :: M03.JAAASON

  let wARR0 = M03.JARR [jSOF0, jSOF2] :: M03.JAAASON
  let wARR1 = M03.JARR [jSOF1, jSON0] :: M03.JAAASON
  let wARR420 = M03.JARR [wARR0, jARR1] :: M03.JAAASON -- aka, make jar not war < 3

  putStrLn ("the jsons: ")

  putStrLn (show jSON0) 
  putStrLn (show jSON1) 
  putStrLn (show jSON2)

  putStrLn ("\n--\n")

  putStrLn (show jSOF0)
  putStrLn (show jSOF1)
  putStrLn (show jSOF2)
 
  putStrLn ("\n--\n")
  
  putStrLn (show jARR0)
  putStrLn (show jARR1)
  putStrLn (show jARR2)
 
  putStrLn ("\n--\n")
  
  putStrLn (show wARR0)
  putStrLn (show wARR1)
  putStrLn (show wARR420)

  putStrLn ("\n-#-#-#-#-#-#-")

  putStrLn ("\nand the go-getters: ")

  putStrLn ("jSON!: " ++ show (M03.jSTR jSON0))

  executionEnd <- getCurrentTime
  
  putStrLn ("----------------------")
  putStrLn ("job (somewhat) done!\nonly took: " ++ show (diffUTCTime executionEnd executionStart))

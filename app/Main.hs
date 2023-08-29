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
		   
import qualified  M01 
                    ( locVicinus
		    , breadth
		    , breadthVless
		    )

main :: IO ()
main = do
 
  executionStart <- getCurrentTime

  -- dumb stuff go here --

  -- clap, clap. proud of Ü
  
  let templ8 = [999, 69, 420, 23, 13, 37, 42]

  let rebeanie = M00.listToTree templ8 :: M00.BinaryTree Int

  putStrLn ("List: ")
  putStrLn (show templ8)

  putStrLn ("---")

  putStrLn ("Tree: ")
  putStrLn (show rebeanie)

  putStrLn ("---")

  putStrLn ("Tree max: " ++ (show (M00.tremax rebeanie)) ++ "\nTree min: " ++ (show (M00.tremin rebeanie)))

  putStrLn ("Tree size: " ++ (show (M00.tresize rebeanie)) ++ "\nTree high: " ++ (show (M00.trehigh rebeanie)))

  putStrLn ("---sumdone---")

  let rebeanie2 = M00.trinsert 1 rebeanie

  putStrLn ("Tree2: ")
  putStrLn (show rebeanie2)

  putStrLn ("Tree max: " ++ (show (M00.tremax rebeanie2)) ++ "\nTree min: " ++ (show (M00.tremin rebeanie2)))

  putStrLn ("Tree size: " ++ (show (M00.tresize rebeanie2)) ++ "\nTree high: " ++ (show (M00.trehigh rebeanie2)))

  putStrLn ("---")

  let rerebeanie = M00.trerrot rebeanie

  putStrLn ("RRotated: ")
  putStrLn (show rerebeanie)

  putStrLn ("---sundown-sumdone---")

  let templ8r = M00.listFrTree rebeanie2

  putStrLn ("List, later: ")
  putStrLn (show templ8r)
  putStrLn ("----------------------")
  putStrLn ("job (somewhat) done!")

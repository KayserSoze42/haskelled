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
		   
main :: IO ()
main = do
 
  executionStart <- getCurrentTime

  -- dumb stuff go here --

  -- clap, clap. proud of Ü
  
  let templ8 = [999, 69, 420, 23, 13, 37, 42]

  let rebeanie = M00.listToTree templ8 :: M00.BinaryTree Int

  putStrLn ("List: ")
  putStrLn (show templ8)

  putStrLn ("Tree: ")
  putStrLn (show rebeanie)

  putStrLn ("Tree max: " ++ (show (M00.tremax rebeanie)) ++ "\nTree min: " ++ (show (M00.tremin rebeanie)))

  putStrLn ("----------------------")
  putStrLn ("job (somewhat) done!")

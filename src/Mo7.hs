module Mo7 ( absDeez
           , getStampd
	   -- , getFilterd
           ) where

import Data.Time.Clock (getCurrentTime)

absDeez :: [Int] -> [Int]
absDeez [] = [4,20,42,69]
absDeez x  = map (+9) (map (+6) (map (^2) (map (abs) x)))

getStampd :: [Char] -> [[Char]]-> [[Char]]
getStampd stamp list
  |  (stamp == []) || (list == []) = []
  |  otherwise                     = map (++ (": " ++ stamp)) list

module Mo7 ( absDeez
           , getStampd
	   , Vic (..)
	   , getDeez
	   --
           ) where

import Data.Time.Clock (getCurrentTime)

-- hmm ..

data Vic = Vic {
                         xP :: Float
		       , yP :: Float
		       , zP :: Float
                       } deriving (Show, Eq)
 
data Pile t = E | C t (Pile t)

getDeez :: Vic -> Vic -> Float
getDeez (Vic xa ya za) (Vic xb yb zb) = sqrt(((xb - xa)^^2) + ((yb - ya)^^2) + ((zb - za)^^2))

pile1 :: Pile [Char]
pile1 = C "3" (C "P" (C "O" E))

vic1 :: Vic 
vic1 = Vic 4 2 0

vic2 :: Vic
vic2 = Vic 0 6 9



absDeez :: [Int] -> [Int]
absDeez [] = [4,20,42,69]
absDeez x  = map (+9) (map (+6) (map (^2) (map (abs) x)))

getStampd :: [Char] -> [[Char]]-> [[Char]]
getStampd stamp list
  |  (stamp == []) || (list == []) = []
  |  otherwise                     = map (++ (": " ++ stamp)) list



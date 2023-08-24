module Mo7 (
           absDeez
           ) where

absDeez :: [Int] -> [Int]
absDeez []     = []
absDeez (x:[]) = (abs x) : []
absDeez (x:xs) = (abs x) : (absDeez xs)

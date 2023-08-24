module Mo7 ( absDeez
           ) where

absDeez :: [Int] -> [Int]
absDeez [] = [4,20,42,69]
absDeez x  = map (+9) (map (+6) (map (^2) (map (abs) x)))



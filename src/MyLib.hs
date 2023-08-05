module MyLib (addd, funkyFunc) where

numberz :: [Int]
numberz = []

addd :: Int -> [Int] -> [Int]
addd x [] = x:[]

funkyFunc :: IO()
funkyFunc = putStrLn (show (addd 6 numberz))

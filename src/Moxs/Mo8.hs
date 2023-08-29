module Mo8 
         ( bubblePleez
	 -- something, something, dark side of de lune
	 , encod3
	 , decod3
	 ) where

import Data.Char hiding 
                      ( isLower
		      , isUpper)

bubbleDeez :: Ord n => [n] -> [n]
bubbleDeez [] = []
bubbleDeez (n:on:ons)
  | n > on    = on : bubbleDeez (n : ons)
  | otherwise = n : bubbleDeez (on : ons)
bubbleDeez ons = ons

-- constain' yoself, innit?

bubblePleez :: Ord n => [n] -> [n]
bubblePleez [] = []
bubblePleez nums = bubblePleez (init deez) ++ [last deez]
                                                          where deez = bubbleDeez nums -- gotteem!

-- ave empress, morituri te salutant .. u 2 cilly tho

isInDeez :: Char -> Char -> Char -> Bool
isInDeez low up char = (char >= low) && (char <= up)

isLow :: Char -> Bool
isLow = isInDeez 'a' 'z'

isUpp :: Char -> Bool
isUpp = isInDeez 'A' 'Z'

--

letTwoInt :: Char -> Char -> Int
letTwoInt uno dos = (ord dos) - (ord uno)

lowTwoInt :: Char -> Int
lowTwoInt = letTwoInt 'a'

highTwoInt :: Char -> Int
highTwoInt = letTwoInt 'A'

--

intTwoLet :: Char -> Int -> Char
intTwoLet uno dos = chr ((ord uno) + dos)

intTwoLowLet :: Int -> Char
intTwoLowLet = intTwoLet 'a'

intTwoHighLet :: Int -> Char
intTwoHighLet = intTwoLet 'A'

--

sh33ft :: Int -> Char -> Char
sh33ft num chr
  | isLow chr = intTwoLowLet ((lowTwoInt chr + num) `mod` 26) -- why 26? ... why not?
  | isUpp chr = intTwoHighLet ((highTwoInt chr + num) `mod` 26)
  | otherwise  = chr

encod3 :: Int -> [Char] -> [Char]
encod3 n xs = [sh33ft n x | x <- xs]

decod3 :: Int -> [Char] -> [Char]
decod3 n = encod3 (-n)

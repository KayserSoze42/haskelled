module Rzr
         (
	 ) where

import Data.List
import Data.Array

-- 1

int2bin :: (Integral n) => n -> [Int]
int2bin 0    = [0]
int2bin 1    = [1]
int2bin n
 | even n    = 0 : int2bin (n `div` 2)
 | otherwise = 1 : int2bin ((n-1) `div` 2)

bin2int :: (Integral n) => [Int] -> n
bin2int [0]  = 0
bin2int [1]  = 1
bin2int (x:xs)
 | x == 0    = 2 * (bin2int xs)
 | otherwise = 1 + 2 * (bin2int xs)

rzrEgge :: (Integral n) => n -> Int
rzrEgge = sum.int2bin

-- 2

rzr0s :: [Int]
rzr0s = 0 : rzr0s

rzr5w0p :: [Int] -> [Int] -> Int
rzr5w0p xees yees = foldl' (+) 0 zees
                                      where zees = zipWith (*) (tail xees) (scanl1 (+) (yees ++ rzr0s))

rzrn3g5 :: [Int] -> [Int] -> Int
rzrn3g5 xees yees = sum rees
                             where rees' = zipWith (*) xees yees
			           rees  = zipWith (*) rees' (cycle [0,1])

-- three fiddy aka zipless xor

rzr35b :: [Int] -> [Int] -> [Int]
rzr35b xees []                 = xees
rzr35b [] yees                 = yees
rzr35b (xs : xees) (ys : yees) = ((xs + ys) `mod` 2) : (rzr35b xees yees)

rzrbp0 :: (Integral n) => n -> n -> (n, Int)
rzrbp0 n m = ((bin2int (rzr35b been beem)), seen)
                                                  where been = int2bin n
						        beem = int2bin m
							seen = ((rzr5w0p been beem) + (rzrn3g5 been beem)) `mod` 2

-- show me.. oh no wait.. put it away! no! Abort! ABORT!

-- bin2chx :: [Int] -> [Char]

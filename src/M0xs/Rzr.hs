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

rzrbp1 :: (Num a, Integral n) => (n, a) -> (n, a) -> (n, a)
rzrbp1 (nx, ax) (mx, bx) = (rx, xx)
                                    where (rx, sx)     = rzrbp0 nx mx 
				          xx
					   | sx == 0   = (ax * bx)
					   | otherwise = negate (ax * bx)

-- show me.. oh no wait.. put it away! no! Abort! ABORT!

bin2chx :: [Int] -> [Char]
bin2chx chx = bin2chx' chx efx lsx
                                   where efx = cycle ['E', 'F']
				         lsx = concatMap (replicate 2) [1..]
					 bin2chx' (n:ns) (e:es) (l:ls) = (res n e l) ++ (bin2chx' ns es ls)
					 bin2chx' [] _ _               = []
					 res n e l
					  | n == 0                     = []
					  | otherwise                  = e : (show l)

rzrsh0 :: (Num a, Show a, Integral n) => (n, a) -> [Char]
rzrsh0 (n, a)
 | n == 0    = show a
 | otherwise = show a ++ "*" ++ (bin2chx (int2bin n))

rzrls :: (Integral n, Num a) => (n, a) -> [(n, a)]
rzrls (n, _) = map n2bp2 (rzr2i4i n)
                                     where n2bp2 i = (i, 1)


rzr2i4i :: (Integral n) => n -> [n]
rzr2i4i n = rzr2i4i' 0 n
                         where rzr2i4i' _ 0 = []
			       rzr2i4i' i n 
			        | even n    = rzr2i4i' (i + 1) (n `div` 2)
				| otherwise = (2 ^ i) : rzr2i4i' (i + 1) ((n - 1) `div` 2)

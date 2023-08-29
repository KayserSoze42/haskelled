module M02 
         ( PoLiSt (..)
	 , fiLiSted
	 , leMapped
	 ) where

data PoLiSt t = E
              | C t (PoLiSt t)

fiLiSted :: (t -> Bool) -> PoLiSt t -> PoLiSt t
fiLiSted _ E                     = E
fiLiSted phil (C x xs)
                     | phil x    = C x (fiLiSted phil xs)
		     | otherwise = fiLiSted phil xs

leMapped :: (a -> b) -> PoLiSt a -> PoLiSt b
leMapped _ E           = E
leMapped phun (C x xs) = C (phun x) (leMapped phun xs)

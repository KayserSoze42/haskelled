module M01 
         ( locVicinus
	 , breadth
	 , breadthVless
	 ) where

-- breadthgh.. lord help us

import Control.Monad (guard)

type Vertx = [Char]

type Egge = (Vertx, Vertx)

type Deezes = Int

type Graff = [Egge]

--

locVicinus :: Graff -> Vertx -> [Vertx]
locVicinus graff vertx = snd (unzip (filter (\(u,_) -> u == vertx) graff ))

breadth :: Graff -> Vertx -> Deezes -> [Vertx]
breadth _ vertx 0         = [vertx]
breadth graff vertx deest = locVicinus graff vertx >>= \u -> breadth graff u (deest - 1)

breadthVless :: Graff -> Vertx -> Deezes -> [Vertx]
breadthVless = justDoEet []
                            where 
			          justDoEet :: [Vertx] -> Graff -> Vertx -> Deezes -> [Vertx]
				  justDoEet seen _ u 0 -- oof.. seen _ u too..
				                     | u `elem` seen = []
						     | otherwise     = [u] -- no ... u
				  justDoEet seen g u k               = locVicinus g u >>= \v -> guard (v `notElem` seen) >> justDoEet (u : seen) g v (k - 1)

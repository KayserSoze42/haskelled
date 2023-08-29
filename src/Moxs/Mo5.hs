module Mo5 
         ( Complex (..)
	 , Tree (..)
	 ) where

data Complex a = Cmplx a (Complex a)
               | Ende
               deriving (Show)

data Tree a = Node a 
                     (Maybe (Tree a))
		     (Maybe (Tree a))
	             deriving (Show)


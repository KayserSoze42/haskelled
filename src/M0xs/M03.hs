module M03 
         ( JAAASON (..)
	 ) where

data JAAASON = JSTR {
                      jSTR :: [Char]
		    }
             | JNMR { 
	              jNMR :: Int
		    }
	     | JBOO {
	              jBOO :: Bool
		    }
	     | JNOO
	     | JOBB {
	              jOBB :: [([Char], JAAASON)]
		    }
	     | JARR {
	              jARR :: [JAAASON]
		    }
	     deriving (Eq, Ord, Show)



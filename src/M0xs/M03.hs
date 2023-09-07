module M03 
         ( JAAASON (..)
	 ) where

import Data.Char 
               ( ord
	       , chr
	       , isHexDigit
	       , isDigit
	       , isSpace
	       , digitToInt
	       ) 

import Data.List 
               ( intercalate
	       )

import Numeric 
             ( showHex
	     )

data JAAASON = JSTR {
                      jSTR :: [Char]
		    }
             | JNMR 
	            { jINT :: Integer
		    , jFRC :: [Int]
		    , jEXP :: Integer
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
	     deriving (Eq, Ord)

instance Show JAAASON where
  show jaaason = case jaaason of
                                JNOO              -> "nun_here"
			        JBOO True         -> "true"
			        JBOO False        -> "false"
			        JSTR txt          -> showMeTheJSoup txt
			        JNMR txt [] 0     -> show txt
			        JNMR txt foo 0    -> show txt ++ "." ++ concatMap show foo
			        JNMR txt [] elmo  -> show txt ++ "e" ++ show elmo
			        JNMR txt foo elmo -> show txt ++ "." ++ concatMap show foo ++ "e" ++ show elmo
			        JARR arr          -> "[" ++ intercalate ", " (map show arr) ++ "]"
			        JOBB obb          -> "{" ++ intercalate ", " (map showKeyVals obb) ++ "}"
			                                                                                 where showKeyVals (key, val) = showMeTheJSoup key ++ ": " ++ show val


showMeTheJSoup :: [Char] -> [Char]
showMeTheJSoup txt = "\""

showMeTheJSauce :: Char -> [Char]
showMeTheJSauce chr = case chr of 
                                  '\''          -> "'"
				  '\"'          -> "\\\"" -- hmmm
				  '\\'          -> "\\\\" 
				  '/'           -> "\\/"  -- ... hmmmm ...
				  '\b'          -> "\\b"
				  '\f'          -> "\\f"  --     .....
				  '\n'          -> "\\n"
				  '\r'          -> "\\r"  -- ...       ...
				  '\t'          -> "\\t"
				  _
				   | isCTRL chr -> "\\u" ++ showMeTheCee chr
				   where showMeTheCee chr = 
				                            let ah = "0000" ++ showHex (ord chr) "" in
							                                               drop (length ah - 4) ah -- ... ... *nodd* 
				  _             -> [chr]
						                                               
												                                                  -- *nodd* ... ...
												                               -- hmh hmh ... ...
															                          -- ... ... mhm mhm

isCTRL :: Char -> Bool
isCTRL chr = chr `elem` ['\0' .. '\31']


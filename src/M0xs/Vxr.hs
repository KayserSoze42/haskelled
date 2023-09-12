module Vxr 
         (
	 ) where

import qualified Rzr 
                   (
		   )

import Data.List
import Data.Char

newtype MxVxr n a = Mv [(n, a)]

-- mxvxr :: (Num a, Eq a) => [(a, [Char])] -> MxVxr Int a
-- mxvxr xs = Mx ()

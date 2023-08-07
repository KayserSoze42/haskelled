module Mo5 (Complex (..)) where

import qualified Data.Maybe as Maybe

data Complex a = Cmplx a (Complex a)
               | Ende
               deriving (Show)


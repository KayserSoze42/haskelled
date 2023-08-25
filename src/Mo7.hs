module Mo7 ( absDeez
           , getStampd
	   , Vic (..)
	   , getDeez
	   --
           ) where

import Data.Time.Clock (getCurrentTime)

-- hmm ..

data Vic = Vic 
             { xP :: Float
	     , yP :: Float
	     , zP :: Float
             } deriving (Show, Eq)
 
data CiVix = CiVix {
                     label :: [Char]
		   , id    :: Int
                   , vic   :: Vic
                   } deriving (Show, Eq)


data VixBinTree = Empty
                | Leaf CiVix
                | Node VixBinTree CiVix VixBinTree
		    deriving (Show, Eq)


data Pile t = E | C t (Pile t)

getDeez :: Vic -> Vic -> Float
getDeez (Vic xa ya za) (Vic xb yb zb) = sqrt(((xb - xa)^^2) + ((yb - ya)^^2) + ((zb - za)^^2))

parseDeez :: [[Char]] -> CiVix
parseDeez (label:id:x:y:z)
  |  label == "alice"     = CiVix "alice" id (Vic x y z)
  |  label == "hatter"    = CiVix "hatter" id (Vic x y z)
  |  label == "mallymkun" = CiVix "mallymkun" id (Vic x y z)
  |  otherwise            = CiVix "caterpillar" id (Vic x y z)
parseDeez [] = CiVix "jabberwacky" 69 (Vic 4 2 0)

parseAllDeez :: [Char] -> [CiVix]
parseAllDeez madfile = map (parseDeez) (map (words) (lines madfile))
parseAllDeez []      = [CiVix "whiterabbit" 13 (Vic 3 1 4)]

put :: CiVix -> VixBinTree -> VixBinTree
put cv Empty = Leaf cv
put (CiVix label id vic) (Leaf (CiVix nlabel nid nvic))
                                                             | id <= nid = Node (Leaf (CiVix label id vic)) (CiVix nlabel nid nvic) Empty
							     | id > nid  = Node (Leaf (CiVix nlabel nid nvic)) (CiVix label id vic) Empty
put (CiVix label id vic) (Node left (CiVix nlabel nid nvic) right)
                                                             | id <= nid = Node (put (CiVix label id vic) left) (CiVix nlabel nid nvic) right
							     | id > nid  = Node left (CiVix nlabel nid nvic) (put (CiVix label id vic) right)

buildDeez :: [CiVix] -> VixBinTree
buildDeez []      = Empty
buildDeez (x:[])  = Leaf x
buildDeez (x:xs)  = put x (buildDeez xs)

pile1 :: Pile [Char]
pile1 = C "3" (C "P" (C "O" E))

vic1 :: Vic 
vic1 = Vic 4 2 0

vic2 :: Vic
vic2 = Vic 0 6 9

absDeez :: [Int] -> [Int]
absDeez [] = [4,20,42,69]
absDeez x  = map (+9) (map (+6) (map (^2) (map (abs) x)))

getStampd :: [Char] -> [[Char]]-> [[Char]]
getStampd stamp list
  |  (stamp == []) || (list == []) = []
  |  otherwise                     = map (++ (": " ++ stamp)) list



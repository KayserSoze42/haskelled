module M00 
         ( BinaryTree (..)
	 , treleft
	 , treight
	 , trehigh
	 , tresize
	 , tremin
	 , tremax
	 , rootree
	 , treelem
	 , trinsert
	 , tremove
	 , listToTree
	 , listFrTree
	 , trelrot
	 , trerrot
	 ) where


-- repetitio est mater studirtme

data BinaryTree it = Empty 
                   | Node (BinaryTree it) it (BinaryTree it)
		   deriving (Show, Read, Eq)

treleft :: BinaryTree it -> BinaryTree it
treleft (Node left _ _) = left

treight :: BinaryTree it -> BinaryTree it
treight (Node _ _ right) = right

trehigh :: BinaryTree it -> Int
trehigh Empty                  = 0
trehigh (Node left curr right) = 1 + max (trehigh left) (trehigh right)

tresize :: BinaryTree it -> Int
tresize Empty                  = 0
tresize (Node left curr right) = (tresize left) + 1 + (tresize right)

tremin :: (Eq it) => BinaryTree it -> it
tremin (Node left curr _)
                        | left /= Empty = tremin left
			| otherwise     = curr

tremax :: (Eq it) => BinaryTree it -> it
tremax (Node _ curr right)
                         | right /= Empty = tremax right
		         | otherwise      = curr

rootree :: it -> BinaryTree it
rootree it = Node Empty it Empty

treelem :: (Ord it) => it -> BinaryTree it -> Bool
treelem it Empty                            = False
treelem it (Node left curr right)
                                | it < curr = treelem it left
				| it > curr = treelem it right
				| otherwise = True

trinsert :: (Ord it) => it -> BinaryTree it -> BinaryTree it
trinsert it Empty                            = rootree it
trinsert it (Node left curr right)
                                | it < curr  = Node (trinsert it left) curr right
				| it > curr  = Node left curr (trinsert it right)

tremove :: (Ord it) => it -> BinaryTree it -> BinaryTree it
tremove it Empty = Empty
tremove it (Node left curr right)
                                | it < curr              = Node (tremove it left) curr right
				| it > curr              = Node left curr (tremove it right)
				| left == Empty          = right
				| right == Empty         = left
				| otherwise              = Node neoSinis neoIt right
				                         where neoIt    = tremax left
							       neoSinis = tremove neoIt left

listToTree :: (Ord its) => [its] -> BinaryTree its
listToTree its = foldr trinsert Empty (reverse its)

listFrTree :: BinaryTree it -> [it]
listFrTree Empty                  = []
listFrTree (Node left curr right) = (listFrTree left) ++ [curr] ++ (listFrTree right)

-- wait ... what?

trelrot :: BinaryTree it -> BinaryTree it
trelrot (Node alpha ita (Node beta itb gamma)) = Node (Node alpha ita beta) itb gamma

trerrot :: BinaryTree it -> BinaryTree it
trerrot (Node (Node alpha ita beta) itb gamma) = Node alpha ita (Node beta itb gamma)

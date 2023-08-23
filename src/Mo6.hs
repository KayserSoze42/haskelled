module Mo6 
         ( TimeStamp
	 , MessageType (..)
	 , LogMessage (..)
	 , MessageBinTree (..)
	 , parseSingle
	 , parseAll
	 , insert
	 , buildTree
	 , inDisOrder
	 ) where

type TimeStamp = Integer

data MessageType = Info
                 | Warning
		 | Error Integer
		 deriving (Show, Eq)

data LogMessage = LogMessage MessageType TimeStamp [Char]
                | Unknown [Char]
		deriving (Show, Eq)

data MessageBinTree = Empty
                    | Leaf LogMessage
                    | Node MessageBinTree LogMessage MessageBinTree
		    deriving (Show, Eq)

parseSingle :: [[Char]] -> LogMessage
parseSingle ("E":n:ts:msg) = LogMessage (Error (read n)) (read ts) (unwords msg)
parseSingle ("I":ts:msg) = LogMessage Info (read ts) (unwords msg)
parseSingle ("W":ts:msg) = LogMessage Warning (read ts) (unwords msg)
parseSingle [] = LogMessage (Error 69) 6969 "420d&69f-gg"

parseAll :: [Char] -> [LogMessage]
parseAll logfile = map (parseSingle) (map (words) (lines logfile))

insert :: LogMessage -> MessageBinTree -> MessageBinTree
insert lm Empty                                                                = Leaf lm
insert (LogMessage msgt ts msg) (Leaf (LogMessage nmsgt nts nmsg))
                                                                   | ts < nts  = Node (Leaf (LogMessage msgt ts msg)) (LogMessage nmsgt nts nmsg) Empty
								   | otherwise = Node (Leaf (LogMessage nmsgt nts nmsg)) (LogMessage msgt ts msg) Empty
insert (LogMessage msgt ts msg) (Node left (LogMessage nmsgt nts nmsg) right)
                                                                   | ts < nts  = Node (insert (LogMessage msgt ts msg) left) (LogMessage nmsgt nts nmsg) right
								   | ts > nts  = Node left (LogMessage nmsgt nts nmsg) (insert (LogMessage msgt ts msg) right)
								   | otherwise = Node left (LogMessage nmsgt nts nmsg) right

buildTree :: [LogMessage] -> MessageBinTree
buildTree []     = Empty
buildTree (x:[]) = Leaf x
buildTree (x:xs) = insert x (buildTree xs)

inDisOrder :: MessageBinTree -> [LogMessage]
inDisOrder (Empty)                = []
inDisOrder (Leaf lm)              = [lm]
inDisOrder (Node left curr right) = inDisOrder left ++ [curr] ++ inDisOrder right


-- whatzUp :: [LogMessage] -> [[Char]]

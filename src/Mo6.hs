module Mo6 
         ( TimeStamp
	 , MessageType (..)
	 , LogMessage (..)
	 , MessageBinTree (..)
	 , parseSingle
	 , parseAll
	 -- , insert
	 -- , buildTree
	 ) where

type TimeStamp = Integer

data MessageType = Info
                 | Warning
		 | Error Integer
		 deriving (Show, Eq)

data LogMessage = LogMessage MessageType TimeStamp [Char]
                | Unknown [Char]
		deriving (Show, Eq)

data MessageBinTree = Leaf
                    | Node MessageBinTree LogMessage MessageBinTree
		    deriving (Show, Eq)

parseSingle :: [[Char]] -> LogMessage
parseSingle ("E":n:ts:msg) = LogMessage (Error (read n)) (read ts) (unwords msg)
parseSingle ("I":ts:msg) = LogMessage Info (read ts) (unwords msg)
parseSingle ("W":ts:msg) = LogMessage Warning (read ts) (unwords msg)
parseSingle [] = LogMessage (Error 69) 6969 "420d&69f-gg"

parseAll :: [Char] -> [LogMessage]
parseAll logfile = map (parseSingle) (map (words) (lines logfile))

-- insert :: LogMessage -> MessageBinTree -> MessageBinTree
-- 

-- buildTree :: [LogMessage] -> MessageBinTree

-- inDisOrder :: MessageBinTree -> [LogMessage]

-- whatzUp :: [LogMessage] -> [[Char]]

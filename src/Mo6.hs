module Mo6 (parseSingle, TimeStamp, MessageType (..), LogMessage (..)) where

type TimeStamp = Integer

data MessageType = Info
                 | Warning
		 | Error Integer
		 deriving (Show, Eq)

data LogMessage = LogMessage MessageType TimeStamp [Char]
                | Unknown [Char]
		deriving (Show, Eq)

data MessageBinTree = Leaf
                    | Node MessageType LogMessage MessageBinTree

parseSingle :: [[Char]] -> LogMessage
parseSingle ("E":n:ts:msg:[]) = LogMessage (Error (read n)) (read ts) msg
parseSingle ("I":ts:msg:[]) = LogMessage Info (read ts) msg
parseSingle ("W":ts:msg:[]) = LogMessage Warning (read ts) msg
parseSingle [] = LogMessage (Error 69) 6969 "420d&69f-gg"

-- parseAll :: [Char] -> [LogMessage]
-- Tuning uncomplete parsing



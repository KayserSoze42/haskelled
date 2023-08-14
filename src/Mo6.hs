module Mo6 (parseSingle, parseAll) where

type TimeStamp = Integer

data MessageType = Info
                 | Warning
		 | Error Integer
		 deriving (Show, Eq)

data LogMessage = LogMessage MessageType TimeStamp [Char]
                | Unknown [Char]
		deriving (Show, Eq)

data MessageBinTree = Leaf
                    | Node MessageType LogMessage MessageTree

parseSingle :: [Char] -> LogMessage
-- Single line parsing

parseAll :: [Char] -> [LogMessage]
-- Tuning uncomplete parsing



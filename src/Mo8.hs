module Mo8 
         (
	 -- something, something, dark side of de lune
	 bubblePleez
	 ) where

bubbleDeez :: Ord n => [n] -> [n]
bubbleDeez [] = []
bubbleDeez (n:on:ons)
  | n > on    = on : bubbleDeez (n : ons)
  | otherwise = n : bubbleDeez (on : ons)
bubbleDeez ons = ons

-- constain' yoself, innit?

bubblePleez :: Ord n => [n] -> [n]
bubblePleez [] = []
bubblePleez nums = bubblePleez (init deez) ++ [last deez]
                                                          where deez = bubbleDeez nums -- gotteem!

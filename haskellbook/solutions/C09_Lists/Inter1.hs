module C09_Lists.Inter1 where

-- 1
myWords :: String -> [String]
myWords [] = [] 
myWords xs = (takeWhile (/= ' ') xs) : (myWords (drop 1 $ dropWhile (/= ' ') xs))

-- 2
myLines :: String -> [String]
myLines [] = [] 
myLines xs = (takeWhile (/= '\n') xs) : (myLines (drop 1 $ dropWhile (/= '\n') xs))

-- 3
splits :: Char -> String -> [String] 
splits _ [] = [] 
splits c xs = (takeWhile (/= c) xs) : (splits c (drop 1 $ dropWhile (/= c) xs))

-------
shouldEqual = [ "Tyger Tyger, burning bright", "In the forests of the night", "What immortal hand or eye", "Could frame thy fearful symmetry?" ]

firstSen = "Tyger Tyger, burning bright\n" 
secondSen = "In the forests of the night\n" 
thirdSen = "What immortal hand or eye\n" 
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

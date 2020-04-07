module ReverseInteger where 

import Data.List (unfoldr)

reverseInteger :: Int -> Int 
reverseInteger n 
  | n < 0     = (-1) * makeNum (splitAndReverse $ (-1) * n)
  | otherwise = makeNum $ splitAndReverse n

makeNum :: [Int] -> Int 
makeNum xs = snd $ foldl (\(len, acc) x -> (len-1, acc + x * 10^len)) (length xs - 1, 0) xs

splitAndReverse :: Int -> [Int] 
splitAndReverse = unfoldr (\x -> if x == 0 then Nothing else Just (x `rem` 10, x `div` 10))

reverseIntegerMain :: IO () 
reverseIntegerMain = do 
  print $ reverseInteger 123 
  print $ reverseInteger (-123) 
  print $ reverseInteger 120

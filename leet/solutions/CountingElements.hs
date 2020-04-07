module CountingElements where 

import Data.List (sort, group)

countElements :: [Int] -> Int 
countElements = countEls . group . sort

countEls :: [[Int]] -> Int 
countEls xss = snd $ foldl (\(prev, acc) xs -> if head xs - head prev == 1 then (xs, acc + length prev) else (xs, acc)) (head xss, 0) xss

countElementsMain :: IO () 
countElementsMain = do 
  print $ countElements [1,2,3]
  print $ countElements [1,1,3,3,5,5,7,7]
  print $ countElements [1,3,2,3,5,0]
  print $ countElements [1,3,1,2,2,5,0]
  print $ countElements [1,1,2,2]
  print $ countElements [1,1,2]

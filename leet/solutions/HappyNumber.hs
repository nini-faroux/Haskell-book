module HappyNumber where 

import qualified Data.Map as Map 
import Data.Maybe (isJust)

happyNum :: Int -> Bool 
happyNum n = go n Map.empty
  where 
    go n acc 
      | sos == 1 = True 
      | isJust $ Map.lookup sos acc = False 
      | otherwise = go sos (Map.insert sos 1 acc) 
      where 
        sos = sumOfSquares n

sumOfSquares :: Int -> Int 
sumOfSquares = sum . map (^2) . splitNum 

splitNum :: Int -> [Int] 
splitNum n = go n [] 
  where 
    go 0 acc = acc 
    go n acc = go (n `div` 10) (n `rem` 10 : acc) 

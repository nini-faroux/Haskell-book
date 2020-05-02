module Easy.HappyNumber where 

import Data.List (unfoldr)
import Data.Maybe (isJust)
import qualified Data.Map as Map 

happyNumber :: Int -> Bool 
happyNumber n = go n Map.empty
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
splitNum = reverse . unfoldr (\x -> if x == 0 then Nothing else Just (x `rem` 10, x `div` 10)) 

happyNumberMain :: IO () 
happyNumberMain = do 
  print $ happyNumber 19
  print $ happyNumber 20

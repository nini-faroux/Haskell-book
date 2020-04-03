module MaxSubArray where 

import Prelude hiding (head, drop)
import Data.List.NonEmpty (head, drop, NonEmpty((:|)))

maxSubArray :: NonEmpty Int -> Int 
maxSubArray xs = snd $ foldr (\x (currMax, max') -> (max x (x + currMax), max (max x (x + currMax)) max')) (head xs, head xs) xs

maxSub :: IO () 
maxSub = do 
  let xs = -2 :| [1,-3,4,-1,2,1,-5,4]
  print $ maxSubArray xs

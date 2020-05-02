module Easy.MoveZeroes where 

import Data.Array.ST
  
moveZeroes :: [Int] -> [Int] 
moveZeroes xs = filter (/= 0) xs ++ replicate (length $ filter (==0) xs) 0

        {-
moveZeroesImperative :: Array Int Int -> Array Int Int 
moveZeroesImperative xs = go xs 0 0 
  where 
    end = snd $ bounds xs 
    go xs idx count 
      | idx == end = xs 
      | otherwise = if xs ! idx /= 0 
                       then go (writeArray xs count (xs ! idx)) (idx + 1) (count + 1) 
                       else go xs (idx + 1) count  -}

-- moveZeroesImperative :: UArray Int Int -> UArray Int Int 
-- moveZeroesImperative xs = 
  -- runSTUArray $ do 

moveZeroesMain :: IO () 
moveZeroesMain = do 
  let xs = [0,12,0,140,1,0,3,12,0]
  print $ moveZeroes xs

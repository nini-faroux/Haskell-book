module Easy.PerfectSquare where

isPerfectSquare :: Int -> Bool
isPerfectSquare n = go 1 (n `div` 2) 
  where 
    go l r 
      | l >= r = l * l == n 
      | m * m == n = True 
      | m * m > n = go l m 
      | otherwise = go (m + 1) r
      where 
        m = l + (r - l) `div` 2

perfectSquareMain :: IO () 
perfectSquareMain = do 
  print $ isPerfectSquare 16
  print $ isPerfectSquare 14 
  print $ isPerfectSquare 25
  print $ isPerfectSquare 256
  print $ isPerfectSquare 4096
  print $ isPerfectSquare 4097

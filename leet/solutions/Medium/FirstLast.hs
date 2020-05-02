module Medium.FirstLast where 

-- in: [5,7,7,8,8,10], target = 8, out: [3,4]
-- in: [5,7,7,8,8,10], target = 6, out: [-1,-1]
-- must be O(log n)

searchRange :: [Int] -> Int -> [Int]
searchRange [] _ = [-1, -1]
searchRange xs target = go xs [0, length xs - 1] 
  where 
    go :: [Int] -> [Int] -> [Int]
    go xs [si, ei] 
      | si == ei = if xs !! si == target then [si, si] else [-1, -1] 
      | xs !! searchIdx == target = landed xs target searchIdx
      | xs !! searchIdx > target  = go xs [si, searchIdx] 
      | otherwise                 = go xs [searchIdx + 1, ei]
      where 
        searchIdx = si + (ei - si) `div` 2

landed :: [Int] -> Int -> Int -> [Int] 
landed xs target idx = [check (-) (reverse $ take idx xs) idx, check (+) (drop (idx+1) xs) idx] 
  where 
    check f [] i = i
    check f (x:xs) i 
      | x == target = check f xs (f i 1) 
      | otherwise   = i

firstLastMain :: IO () 
firstLastMain = do 
  let xs = [2,3,5,7,7,8,8,10,10,10,10]
  let ys = [2,2,2,3,5,7,7,8,8,10,10,10,10]
  let zs = [5,7,7,8,8,10,10,11]
  print $ searchRange [] 8
  print $ searchRange zs 13
  print $ searchRange zs 6
  print $ searchRange zs 8 
  print $ searchRange ys 5 
  print $ searchRange xs 10
  print $ searchRange xs 2
  print $ searchRange ys 2
  print $ searchRange ys 3
  print $ searchRange zs 11

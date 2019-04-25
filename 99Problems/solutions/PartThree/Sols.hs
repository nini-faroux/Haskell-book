module PartThree.Sols where 

-- 21 
-- insertAt 'X' "abcd" 2
-- "aXbcd"
insertAt :: a -> [a] -> Int -> [a] 
insertAt x xs k = take (k-1) xs ++ [x] ++ drop (k-1) xs

-- 22
-- > range 4 9
-- [4,5,6,7,8,9]
range :: Int -> Int -> [Int] 
range s e = [s..e]

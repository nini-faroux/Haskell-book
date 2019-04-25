module PartThree.Sols where 

-- 21 
-- insertAt 'X' "abcd" 2
-- "aXbcd"
insertAt :: a -> [a] -> Int -> [a] 
insertAt x xs k = take (k-1) xs ++ [x] ++ drop (k-1) xs

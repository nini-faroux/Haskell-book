module Chap12.EitherLib where

-- 1
lefts' :: [Either a b] -> [a] 
lefts' xs = foldr (\x y -> if isLeft x then fromLeft x : y else y) [] xs

isLeft :: Either a b -> Bool 
isLeft (Left _) = True 
isLeft _ = False 

fromLeft :: Either a b -> a 
fromLeft (Left x) = x

-- 2 
rights' :: [Either a b] -> [b] 
rights' xs = foldr (\a b -> if isRight a then fromRight a : b else b) [] xs

isRight :: Either a b -> Bool 
isRight (Right _) = True 
isRight _ = False 

fromRight :: Either a b -> b
fromRight (Right x) = x

-- 3
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' [] = ([], [])
partitionEithers' xs = go xs [] [] 
  where
    go [] ls rs = (ls, rs)
    go (x:xs) ls rs
      | isLeft x  = go xs (fromLeft x : ls) rs 
      | otherwise = go xs ls (fromRight x : rs)

-- 4
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing 
eitherMaybe' f (Right x) = Just $ f x

-- 5
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ g (Right x) = g x

-- 6
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' g e = either' (\_ -> Nothing) (\x -> Just (g x)) e

module Chap12.MaybeLib where

-- 1
isJust :: Maybe a -> Bool 
isJust (Just _) = True 
isJust _ = False 

isNothing :: Maybe a -> Bool 
isNothing Nothing = True 
isNothing _ = False

-- 2
maybeeCat :: b -> (a -> b) -> Maybe a -> b
maybeeCat x _ Nothing = x
maybeeCat x f (Just y) = f y

-- 3
fromMaybe :: a -> Maybe a -> a 
fromMaybe x Nothing = x 
fromMaybe _ (Just y) = y

fromMaybe2 :: a -> Maybe a -> a 
fromMaybe2 x m = maybeeCat x id m

-- 4
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing 
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = [] 
maybeToList (Just x) = [x]

-- 5
fromJustt :: Maybe a -> a 
fromJustt (Just x) = x

catMaybees :: [Maybe a] -> [a]
catMaybees xs = foldr (\a b -> if isNothing a then b else fromJustt a : b) [] xs

-- 6 
flipMaybee :: [Maybe a] -> Maybe [a] 
flipMaybee [] = Nothing 
flipMaybee xs = go xs [] 
  where 
    go [] acc = Just $ reverse acc 
    go (x:xs) acc 
      | isNothing x = Nothing
      | otherwise   = go xs (fromJustt x : acc)

module C12_SigAdv.StringProcessing where

-- 1
-- $ replaceThe "the cow loves us"
-- "a cow loves us"

notThe :: String -> Maybe String
notThe xs
  | xs == "the" || xs == "The" = Nothing
  | otherwise = Just xs

replaceThe :: String -> String 
replaceThe xs = go (words xs) where
  go xxs = unwords $ foldr (\x y -> if notThe x == Nothing then "a" : y else x : y) [] xxs

-- 2
-- $ countTheBeforeVowel "the evil cow"
-- 1
vowels :: [(Char, Int)]
vowels = [('a', 1), ('e', 1), ('i', 1), ('o',1), ('u', 1)]

vowelStart :: String -> Bool
vowelStart xs = foldr (\a _ -> if lookup a vowels == Nothing then False else True) False xs

countTheBeforeVowel :: String -> Int
countTheBeforeVowel [] = 0
countTheBeforeVowel xs = go (words xs) Nothing []
  where
    go [] prev acc = length acc
    go (x:xs) prev acc
      | prev == Nothing && (vowelStart x) = go xs (notThe x) (x:acc)
      | otherwise = go xs (notThe x) acc

-- 3
-- $ countVowels "the cow"
-- 2
countVowels :: String -> Int
countVowels xs = foldr (\x y -> if lookup x vowels == (Just 1) then 1 + y else y) 0 xs

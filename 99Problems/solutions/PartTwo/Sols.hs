module PartTwo.Sols where 

import PartOne.Sols
import Control.Monad (join)

-- 11
-- encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

data Encode a =
    Single a 
  | Multiple Int a
  deriving Show 

encodeMod :: Eq a => [a] -> [Encode a] 
encodeMod xs = enc <$> pack xs

enc :: [a] -> Encode a 
enc xs 
  | length xs > 1 = Multiple (length xs) (head xs)
  | otherwise     = Single (head xs)

-- 12
-- decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"

decodeMod :: [Encode a] -> [a] 
decodeMod = concatMap decode 

decode :: Encode a -> [a]
decode (Multiple n x) = replicate n x
decode (Single x) = [x]

-- 13 
encodeDirect :: Eq a => [a] -> [Encode a] 
encodeDirect [] = []
encodeDirect xs = go xs (head xs) 0 [] 
  where 
    go [] prev count acc = app prev count acc 
    go (x:xs) prev count acc 
      | x == prev  = go xs prev (count+1) acc
      | count == 1 = go xs x 1 (acc ++ [Single prev])
      | otherwise  = go xs x 1 (acc ++ [Multiple count prev])

app :: a -> Int -> [Encode a] -> [Encode a]
app p c a
   | c == 1    = a ++ [Single p]
   | otherwise = a ++ [Multiple c p]

-- 14
-- * (dupli '(a b c c d))
-- (A A B B C C C C D D)
dupli :: Eq a => [a] -> [a]
dupli xs = join $ zipWith replicate (fst <$> dupEnc xs) (head . snd <$> dupEnc xs)

dupEnc :: Eq a => [a] -> [(Int, [a])]
dupEnc xs = zip ((*2) . length <$> pack xs) (pack xs)

-- 15
-- repli "abc" 3
-- "aaabbbccc"
repli :: [a] -> Int -> [a] 
repli xs n = xs >>= replicate n 

-- 16
-- dropEvery "abcdefghik" 3
-- "abdeghk"
dropEvery :: [a] -> Int -> [a] 
dropEvery xs n = go xs 1 []
  where 
    go [] _ acc = acc 
    go (x:xs) cnt acc
      | cnt == n = go xs 1 acc 
      | otherwise = go xs (cnt+1) (acc++[x])

-- 17
-- > split "abcdefghik" 3
-- ("abc", "defghik")

split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

-- 18
-- Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list
-- slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"

slice :: [a] -> Int -> Int -> [a] 
slice xs i k = take (k-i+1) $ drop (i-1) xs

--------------------

partTwoTest :: IO () 
partTwoTest = do
    putStrLn "encodeModified \"aaaabccaadeeee\""
    print $ encodeMod "aaaabccaadeeee"

    putStrLn "decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']"
    print $ decodeMod [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']

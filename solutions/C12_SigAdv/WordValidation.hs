module C12_SigAdv.WordValidation where

newtype Word' = 
    Word' String 
    deriving (Show, Eq)

vowelss :: [(Char, Int)]
vowelss = [('a', 1), ('e', 1), ('i', 1), ('o', 1), ('u', 1)]

-- write a function that counts the number of vowels in a string and the number of consonants. 
-- If the number of vowels exceeds the number of consonants, the function returns Nothing.
mkWord :: String -> Maybe Word'
mkWord [] = Nothing 
mkWord xs = go xs [] [] 
  where
    go [] vs cs = if (length vs > length cs) then Nothing else Just $ Word' xs
    go (x:xs) vs cs 
      | lookup x vowelss == Nothing = go xs vs (x:cs)
      | otherwise                   = go xs (x:vs) cs

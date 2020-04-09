module BackspaceCompare where 

import Data.Char (isSpace)

backspaceCompare :: String -> String -> Bool 
backspaceCompare s t = go (reverse s) (reverse t) 
  where 
    go [] [] = True 
    go s []  = isSpace . fst $ increment s 
    go [] t  = isSpace . fst $ increment t
    go s t 
      | sChar == tChar = go sStr tStr 
      | otherwise = False 
      where 
        (sChar, sStr) = increment s 
        (tChar, tStr) = increment t

increment :: String -> (Char, String)
increment s = go s 0 
  where 
    go [] _ = (' ', [])
    go (x:xs) hash 
      | x == '#'  = go xs (hash + 1) 
      | hash > 0  = go xs (hash - 1) 
      | otherwise = (x, xs)

backspaceCompareMain :: IO () 
backspaceCompareMain = do 
  print $ backspaceCompare "ab#c" "ad#c"
  print $ backspaceCompare "ab##" "c#d#"
  print $ backspaceCompare "a##c" "#a#c"
  print $ backspaceCompare "a#c" "b"
  print $ backspaceCompare "abc#cd##cd" "a#a#ab#bcd##cc#d"
  print $ backspaceCompare "abc#cd##cd" "a#a#ab#bcd##cc#dd"

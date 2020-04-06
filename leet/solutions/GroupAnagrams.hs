module GroupAnagrams where 

import Data.List (sort) 
import Data.Maybe (isNothing, fromJust)
import Lens.Micro (ix, (&), (.~), (^.))
import qualified Data.Map as Map 

groupAnagrams :: [String] -> [[String]] 
groupAnagrams xs = foldl add (replicate (Map.size indices) []) xs
  where 
    add xs x = xs & ix (idx x) .~ (x : xs !! idx x)
    indices = wordsToIndex $ sort <$> xs
    idx x = fromJust $ Map.lookup (sort x) indices 

wordsToIndex :: [String] -> Map.Map String Int 
wordsToIndex = snd . foldl (\(idx, m) x -> if isNothing (Map.lookup x m) then (idx+1, Map.insert x idx m) else (idx, m)) (0, Map.empty) 

groupAnagramsMain :: IO () 
groupAnagramsMain = do 
  print $ groupAnagrams ["eat", "tea", "tan", "ate", "nat", "bat"]
  print $ groupAnagrams []

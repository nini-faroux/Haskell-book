module Medium.FreqSort where 

import qualified Data.Map as Map
import Data.Ord (Down(..), comparing)
import Data.List (sortOn)

frequencySort :: String -> String
frequencySort = makeString . sortByValue . makeCount

makeString :: [(Char, Int)] -> String 
makeString = concatMap (\(c, i) -> replicate i c) 

sortByValue :: [(Char, Int)] -> [(Char, Int)]
sortByValue = sortOn (Down . snd) 

makeCount :: String -> [(Char, Int)]
makeCount xs = Map.toList $ Map.fromListWith (+) [(c, 1) | c <- xs]

frequencySortMain :: IO () 
frequencySortMain = do 
    print $ frequencySort "tree" 
    print $ frequencySort "Aabb"
    print $ frequencySort "cccaaa"

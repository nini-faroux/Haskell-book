module Lists_Three.Sols where 

import System.Random (randomRIO)
import Data.Maybe (isNothing)

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

-- 23
-- Extract a given number of randomly selected elements from a list.
-- λ> rnd_select "abcdefgh" 3 >>= putStrLn
-- eda
rndSelect :: [a] -> Int -> IO [a] 
rndSelect xs n = go xs 0 [] (randomRIO (0, length xs-1)) 
  where 
    go xs cnt acc rg
      | cnt == n  = return acc 
      | otherwise = do 
           rnd <- rg 
           go xs (cnt+1) (acc ++ [xs !! rnd]) rg

-- 24
-- Lotto: Draw N different random numbers from the set 1..M.
-- λ> diff_select 6 49
-- [23,1,17,33,21,37]
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = go 0 [] (randomRIO (1, m)) 
  where 
    go cnt acc rg 
      | cnt == n  = return acc 
      | otherwise = do
          rnd <- rg 
          go (cnt+1) (acc ++ [rnd]) rg

-- 25
-- Generate a random permutation of the elements of a list.
-- λ> rnd_permu "abcdef"
-- "badcef"
randPermu :: Ord a => [a] -> IO [a] 
randPermu xs = fmap snd <$> indices xs 

indices :: Ord a => [a] -> IO [(Int, a)] 
indices xs = go xs (randomRIO (0, length xs - 1)) [] 
  where 
    go [] _ acc = return $ qsort acc
    go li@(x:xs) rg acc = do
            r <- rg 
            if isNothing $ lookup r acc 
               then go xs rg (acc ++ [(r, x)]) 
               else go li rg acc 

qsort :: Ord a => [a] -> [a] 
qsort [] = [] 
qsort (pv:xs) = qsort [x | x <- xs, x < pv] ++ [pv] ++ qsort [x | x <- xs, x >= pv]

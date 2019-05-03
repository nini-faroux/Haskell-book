module Trees.Trie where 

import Data.Maybe (isNothing, fromJust)

data Trie = 
  TNode EoW [(Char, Trie)]
  deriving (Eq, Show)

type EoW = Bool

search :: String -> Trie -> Bool 
search [] (TNode b _) = b
search (x:_) (TNode b []) = False
search (x:xs) (TNode b ys) 
  | isNothing $ lookup x ys = False 
  | otherwise = search xs (fromJust $ lookup x ys)

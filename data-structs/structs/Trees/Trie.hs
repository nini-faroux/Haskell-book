module Trees.Trie where 

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

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

-- testing 
instance EqProp Trie where 
  (=-=) = eq 

instance Arbitrary Trie where
  arbitrary = trieGen

trieGen :: Gen Trie
trieGen = do
  b <- (arbitrary :: Gen Bool)
  c <- arbitrary 
  oneof [return $ TNode b [], return $ TNode b [c]]

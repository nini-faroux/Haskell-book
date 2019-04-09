module Trees.BinaryTree where

data BinTree a =
    Leaf 
  | Node (BinTree a) a (BinTree a)
  deriving Show 

inOrder :: BinTree a -> [a] 
inOrder Leaf = []
inOrder (Node l x r) = inOrder l ++ [x] ++ inOrder r

preOrder :: BinTree a -> [a] 
preOrder Leaf = [] 
preOrder (Node l x r) = [x] ++ preOrder l ++ preOrder r 

postOrder :: BinTree a -> [a] 
postOrder Leaf = []
postOrder (Node l x r) = postOrder l ++ postOrder r ++ [x] 

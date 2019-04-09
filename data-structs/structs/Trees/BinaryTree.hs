module Trees.BinaryTree where

data BinTree a =
    Leaf 
  | Node (BinTree a) a (BinTree a)
  deriving Show 

instance Functor BinTree where 
  fmap _ Leaf = Leaf 
  fmap f (Node l x r) = Node (f <$> l) (f x) (f <$> r)

instance Applicative BinTree where 
  pure x = Node Leaf x Leaf 
  Leaf <*> _ = Leaf 
  _ <*> Leaf = Leaf 
  (Node lf f rf) <*> (Node l x r) = Node (lf <*> l) (f x) (rf <*> r)

foldTree :: (b -> a -> b) -> b -> BinTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node l x r) = foldTree f (foldTree f z l `f` x) r

inOrder :: BinTree a -> [a] 
inOrder Leaf = []
inOrder (Node l x r) = inOrder l ++ [x] ++ inOrder r

preOrder :: BinTree a -> [a] 
preOrder Leaf = [] 
preOrder (Node l x r) = [x] ++ preOrder l ++ preOrder r 

postOrder :: BinTree a -> [a] 
postOrder Leaf = []
postOrder (Node l x r) = postOrder l ++ postOrder r ++ [x] 

insert :: Ord a => a -> BinTree a -> BinTree a 
insert x Leaf = Node Leaf x Leaf 
insert x (Node l y r) 
  | x >= y    = Node l y (insert x r) 
  | otherwise = Node (insert x l) y r

testTree :: BinTree Integer 
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

appTree :: BinTree (Integer -> Integer) 
appTree = Node (Node Leaf (*2) Leaf) (+10) (Node Leaf (*4) Leaf)

testBinTree :: IO () 
testBinTree = do 
    print testTree
    print $ (*2) <$> testTree
    print $ appTree <*> testTree

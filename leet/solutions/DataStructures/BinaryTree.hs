module DataStructures.BinaryTree where 

data BinaryTree a = 
    Leaf 
  | Node a (BinaryTree a) (BinaryTree a) 
  deriving (Eq, Show)

instance Semigroup a => 
        Semigroup (BinaryTree a) where 
  Leaf <> t = t 
  t <> Leaf = t 
  (Node x l r) <> (Node y l' r') = Node (x <> y) (l <> l') (r <> r')

instance Monoid a => 
        Monoid (BinaryTree a) where 
  mempty = Leaf 
  mappend = (<>)

instance Functor BinaryTree where 
  fmap _ Leaf = Leaf 
  fmap f (Node x l r) = Node (f x) (f <$> l) (f <$> r)

buildTree :: Ord a => [a] -> BinaryTree a 
buildTree = foldl (flip btInsert) Leaf 

btInsert :: Ord a => a -> BinaryTree a -> BinaryTree a 
btInsert x Leaf = Node x Leaf Leaf
btInsert x (Node y l r) 
  | x <= y = Node y (btInsert x l) r 
  | otherwise = Node y l (btInsert x r) 

btElem :: Ord a => a -> BinaryTree a -> Bool
btElem _ Leaf = False 
btElem x (Node y l r) 
  | x == y = True 
  | x < y = btElem x l 
  | otherwise = btElem x r

foldBinaryTree :: (a -> b -> b) -> b -> BinaryTree a -> BinaryTree b 
foldBinaryTree f z Leaf = Leaf
foldBinaryTree f z (Node x l r) = Node x' (foldBinaryTree f x' l) (foldBinaryTree f x' r) 
  where 
    x' = f x z

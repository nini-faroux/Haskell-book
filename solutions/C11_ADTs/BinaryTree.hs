module C11_ADTs.BinaryTree where

data BinaryTree a = 
    Leaf 
  | Node (BinaryTree a) a (BinaryTree a) 
  deriving (Eq, Show, Ord)

-- 3, fold for trees 
foldTree :: (b -> a -> b) -> b -> BinaryTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node left x right) = foldTree f ((foldTree f z left) `f` x) right

-- 1
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf 
mapTree f (Node left x right) = Node (mapTree f left) (f x) (mapTree f right)

-- 2, trees to lists 
preorder :: BinaryTree a -> [a] 
preorder Leaf = [] 
preorder (Node left x right) = [x] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a] 
inorder Leaf = [] 
inorder (Node left x right) = inorder left ++ [x] ++ inorder right

postorder :: BinaryTree a -> [a] 
postorder Leaf = []
postorder (Node left x right) = postorder left ++ postorder right ++ [x]

-- given
insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' x Leaf = Node Leaf x Leaf
insert' x (Node left y right) 
  | x < y     = Node (insert' x left) y right
  | x > y     = Node left y (insert' x right)
  | otherwise = Node left x right

-- tests 
testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO () 
testPreorder = if preorder testTree == [2, 1, 3] then putStrLn "Preorder fine!" else putStrLn "Bad news bears."

testInorder :: IO () 
testInorder = if inorder testTree == [1, 2, 3] then putStrLn "Inorder fine!" else putStrLn "Bad news bears."

testPostorder :: IO () 
testPostorder = if postorder testTree == [1, 3, 2] then putStrLn "Postorder fine!" else putStrLn "postorder failed check"
  
testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = if mapTree (+1) testTree' == mapExpected then print "yup okay!" else error "test failed!"

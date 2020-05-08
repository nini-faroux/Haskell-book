module Easy.BinaryTreeCousins where 

import DataStructures.BinaryTree 

isCousins :: BinaryTree Int -> Int -> Int -> Bool 
isCousins t x y = cousins (depthAndParent t x) (depthAndParent t y) 

cousins :: (Int, Int) -> (Int, Int) -> Bool 
cousins (d1, p1) (d2, p2) = d1 == d2 && p1 /= p2

depthAndParent :: BinaryTree Int -> Int -> (Int, Int) 
depthAndParent t@(Node pv _ _) x = go t pv 0
    where 
      go Leaf _  _ = (-1, -1) 
      go (Node v l r) pv d
        | v == x = (d, pv) 
        | otherwise = max (go l v (d + 1)) (go r v (d + 1))

isCousinsMain :: IO () 
isCousinsMain = do 
  let t1 = buildTree [5, 2, 4, 8, 9] 
  let t2 = buildTree [5, 2, 8]
  print $ isCousins t1 9 4
  print $ isCousins t1 2 8


module Easy.BTHeight where 

import DataStructures.BinaryTree 

height :: BinaryTree a -> Int 
height Leaf = 0 
height (Node _ l r) = max 1 mlr 
  where mlr = max (1 + height l) (1 + height r)

heightMain :: IO () 
heightMain = do 
  let t = buildTree [14,2,33,4,5,16]
  print t
  print $ height t

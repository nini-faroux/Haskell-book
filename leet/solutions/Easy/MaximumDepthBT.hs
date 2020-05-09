module Easy.MaximumDepthBT where 

import DataStructures.BinaryTree

maximumDepth :: BinaryTree a -> Int 
maximumDepth Leaf = 1 
maximumDepth (Node _ l r) = max (1 + maximumDepth l) (1 + maximumDepth r)


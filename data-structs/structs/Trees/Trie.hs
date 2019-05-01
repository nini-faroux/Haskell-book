module Trees.Trie where 

data Trie = 
  Node Leaf [(Char, Trie)]
  deriving (Eq, Show)

type Leaf = Bool



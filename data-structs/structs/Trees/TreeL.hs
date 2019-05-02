{-# LANGUAGE InstanceSigs #-}

module Trees.TreeL where 

data TreeL a = 
  NodeL a [TreeL a] 
  deriving (Eq, Show)

instance Functor TreeL where
  fmap :: (a -> b) -> TreeL a -> TreeL b
  fmap f (NodeL x xs) = NodeL (f x) (fmap f <$> xs)



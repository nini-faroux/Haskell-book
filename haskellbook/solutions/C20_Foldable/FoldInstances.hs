{-# LANGUAGE InstanceSigs #-}

module C20_Foldable.FoldInstances where

-- 1 
newtype Constant a b = Constant a deriving (Eq, Show)

instance Foldable (Constant a) where
  foldr :: (a1 -> b -> b) -> b -> Constant a a1 -> b
  foldr f z (Constant x) = z

-- 2
data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldr :: (a1 -> b -> b) -> b -> Two a a1 -> b
  foldr f z (Two x y) = f y z

-- 3
data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a a1) where
  foldr :: (a2 -> b -> b) -> b -> Three a a1 a2 -> b
  foldr f z (Three a b c) = f c z

-- 4
data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  foldr :: (a1 -> b -> b) -> b -> Three' a a1 -> b
  foldr f z (Three' a b c) = f b z

-- 5 
data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  foldr :: (a1 -> b -> b) -> b -> Four' a a1 -> b
  foldr f z (Four' a b c d) = f d z

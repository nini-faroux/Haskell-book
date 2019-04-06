{-# LANGUAGE InstanceSigs #-}

module C25_ComposingTypes.Bifunctors where

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (c -> d) -> p a c -> p a d
  second = bimap id

-- 1 
data Deux a b = Deux a b deriving (Eq, Show)

instance Bifunctor Deux where 
  bimap :: (a -> b) -> (c -> d) -> Deux a c -> Deux b d
  bimap f g (Deux x y) = Deux (f x) (g y)

-- 2
newtype Const a b = Const a deriving (Eq, Show)

instance Bifunctor Const where
  bimap :: (a -> b) -> (c -> d) -> Const a c -> Const b d
  bimap f g (Const x) = Const (f x)

-- 3
data Drei a b c = Drei a b c deriving (Eq, Show)

instance Bifunctor (Drei a) where 
  bimap f g (Drei x y z) = Drei x (f y) (g z)

-- 4
data SuperDrei a b c = SuperDrei a b deriving (Eq, Show)

instance Bifunctor (SuperDrei a) where
  bimap f g (SuperDrei x y) = SuperDrei x (f y)

-- 5
newtype SemiDrei a b c = SemiDrei a deriving (Eq, Show)

instance Bifunctor (SemiDrei a) where
  bimap f g (SemiDrei x) = SemiDrei x

-- 6 
data Quadriceps a b c d = Quadz a b c d deriving (Eq, Show) 

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadz a b c d) = Quadz a b (f c) (g d)

-- 7 
data Eitherr a b = 
    Leftt a 
  | Rightt b 
  deriving (Eq, Show) 

instance Bifunctor Eitherr where 
  bimap :: (a -> b) -> (c -> d) -> Eitherr a c -> Eitherr b d
  bimap f _ (Leftt x) = Leftt (f x)
  bimap _ g (Rightt x) = Rightt (g x)

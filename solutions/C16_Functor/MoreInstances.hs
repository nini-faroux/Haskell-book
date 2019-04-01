{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module C16_Functor.MoreInstances where

-- 1
data Quant a b =
    Finance 
  | Desk a 
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance 
  fmap _ (Desk x) = Desk x
  fmap f (Bloor x) = Bloor (f x)

-- 2
data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K x) = K x

-- 3
newtype Flip f a b = 
    Flip (f b a)
    deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap :: (a1 -> b) -> Flip K a a1 -> Flip K a b
  fmap f (Flip (K a)) = Flip $ K (f a)

-- 4 
data EvilGoatConst a b = GoatyConst b deriving Show

instance Functor (EvilGoatConst a) where
  fmap :: (a1 -> b) -> EvilGoatConst a a1 -> EvilGoatConst a b
  fmap f (GoatyConst x) = GoatyConst (f x)

-- 5
data LiftItOut f a = 
    LiftItOut (f a) 
    deriving Show

instance Functor f =>
        Functor (LiftItOut f) where
  fmap :: (a -> b) -> LiftItOut f a -> LiftItOut f b
  fmap g (LiftItOut f) = LiftItOut $ fmap g f

-- 6
data Parappa f g a = 
    DaWrappa (f a) (g a)
    deriving Show

instance (Functor f, Functor g) => 
        Functor (Parappa f g) where
  fmap :: (a -> b) -> Parappa f g a -> Parappa f g b
  fmap h (DaWrappa f g) = DaWrappa (fmap h f) (fmap h g)

-- 7 
data IgnoreOne f g a b = 
    IgnoreSomething (f a) (g b) 
    deriving Show

instance (Functor f, Functor g) => 
        Functor (IgnoreOne f g a) where
  fmap h (IgnoreSomething f g) = IgnoreSomething f (fmap h g)

-- 8 
data Notorious g o a t = 
    Notorious (g o) (g a) (g t) 
    deriving Show 

instance Functor g => 
        Functor (Notorious g o a) where
   fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 9
data List a = 
    Nil 
  | Cons a (List a)
  deriving Show 

instance Functor List where
  fmap _ Nil = Nil 
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- 10 
data GoatLord a = 
    NoGoat 
  | OneGoat a 
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) 
  deriving Show

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat 
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

-- 11 
data TalkToMe a = 
    Halt 
  | Print String a 
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt 
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g) = Read $ \x -> f (g x)

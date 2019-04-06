module C17_Applicative.Identity where

newtype Ident' a = Ident' a deriving Show 

instance Functor Ident' where
  fmap f (Ident' x) = Ident' (f x)

instance Applicative Ident' where
  pure = Ident' 
  (Ident' f) <*> (Ident' x) = Ident' (f x)

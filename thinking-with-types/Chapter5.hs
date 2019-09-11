{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module HeterolistTWTs where 

import Data.Kind (Type) 
import Data.Monoid ((<>))

data HList (ts :: [Type]) where 
  HNil :: HList '[] 
  (:#) :: t -> HList ts -> HList (t ': ts) 

infixr 5 :#

hLength :: HList ts -> Int 
hLength HNil = 0 
hLength (_ :# ts) = 1 + hLength ts 

hHead :: HList (t ': ts) -> t 
hHead (t :# _) = t 

instance Eq (HList '[]) where 
  HNil == HNil = True 

instance (Eq t, Eq (HList ts)) 
            => Eq (HList (t ': ts)) where 
  (a :# as) == (b :# bs) = a == b && as == bs

instance Ord (HList '[]) where 
  compare HNil HNil = EQ

-- EQ <> EQ = EQ 
-- EQ <> LT <> GT = LT 
-- EQ <> GT <> LT = GT 
instance (Ord t, Ord (HList ts)) 
           => Ord (HList (t ': ts)) where 
  compare (x :# xs) (y :# ys) = compare x y <> compare xs ys

instance Show (HList '[]) where 
  show HNil = "[]"

instance (Show t, Show (HList ts)) 
          => Show (HList (t ': ts)) where 
  show (x :# xs) = show x ++ " " ++ show xs

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}

module S06_TypeFamilies.Exercises where

import Data.Kind (Constraint, Type)

-- | Before we get started, let's talk about the @TypeOperators@ extension. All
-- this does is allow us to write types whose names are operators, and write
-- regular names as infix names with the backticks, as we would at the value
-- level.

{- ONE -}

data Nat = Z | S Nat

-- | a. Use the @TypeOperators@ extension to rewrite the 'Add' family with the
-- name '+':
type family (+) (x :: Nat) (y :: Nat) :: Nat where 
  'Z + y = y 
  ('S n) + y = 'S (n + y)

-- | b. Write a type family '**' that multiplies two naturals using '(+)'. Which
-- extension are you being told to enable? Why?
-- 'UndecidableInstances' - it's not a total function

type family (**) (x :: Nat) (y :: Nat) :: Nat where 
  'Z ** y = 'Z 
  ('S x) ** y = y + (x ** y)

-- | c. Write a function to add two 'SNat' values.

data SNat (value :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

addSNats :: SNat x -> SNat y -> SNat (x + y)
addSNats SZ y = y
addSNats (SS x) y = SS $ addSNats x y

{- TWO -}

data Vector (count :: Nat) (a :: Type) where
  VNil  :: Vector 'Z a
  VCons :: a -> Vector n a -> Vector ('S n) a

instance Functor (Vector c) where 
  fmap :: (a -> b) -> Vector m a -> Vector m b 
  fmap _ VNil = VNil
  fmap f (VCons x xs) = VCons (f x) (fmap f xs)

-- | a. Write a function that appends two vectors together. What would the size
-- of the result be?

append :: Vector m a -> Vector n a -> Vector (m + n) a
append VNil ys = ys 
append (VCons x xs) ys = VCons x (append xs ys)

-- | b. Write a 'flatMap' function that takes a @Vector n a@, and a function
-- @a -> Vector m b@, and produces a list that is the concatenation of these
-- results. This could end up being a deceptively big job.

flatMap :: Vector n a -> (a -> Vector m b) -> Vector (n ** m) b
flatMap xs f = flatten $ fmap f xs

flatten :: Vector n (Vector m a) -> Vector (n ** m) a 
flatten VNil = VNil
flatten (VCons xs xss) = append xs (flatten xss)

{- THREE -}

-- | a. More boolean fun! Write the type-level @&&@ function for booleans.
type family (&&) (a :: Bool) (b :: Bool) :: Bool where 
  'True && 'True = 'True 
  _ && _         = 'False

-- | b. Write the type-level @||@ function for booleans.
type family (||) (a :: Bool) (b :: Bool) :: Bool where 
  'False || 'False = 'False 
  _ || _ = 'True 

-- | c. Write an 'All' function that returns @'True@ if all the values in a
-- type-level list of boleans are @'True@.
type family All (xs :: [Bool]) :: Bool where 
  All '[] = 'True
  All ('True ': ys) = All ys 
  All ('False ': _) = 'False 

{- FOUR -}

-- | a. Nat fun! Write a type-level 'compare' function using the promoted
-- 'Ordering' type.
type family Compare (x :: Nat) (y :: Nat) :: Ordering where 
  Compare 'Z 'Z         = 'EQ 
  Compare 'Z ('S x)     = 'LT 
  Compare ('S x) 'Z     = 'GT 
  Compare ('S x) ('S y) = Compare x y

type family (===) (n :: Nat) (m :: Nat) :: Bool where 
  'Z === 'Z = 'True 
  'Z === m = 'False 
  n === 'Z  = 'False 
  'S n === 'S m = n === m

-- | b. Write a 'Max' family to get the maximum of two natural numbers.
type family OrdToBool (o :: Ordering) :: Bool where 
  OrdToBool 'EQ = 'True
  OrdToBool 'GT = 'True 
  OrdToBool 'LT = 'False

type family GreaterEqual (x :: Nat) (y :: Nat) :: Bool where 
  GreaterEqual x y = OrdToBool (Compare x y)

type family If (c :: Bool) (x :: Nat) (y :: Nat) where
  If 'True x _  = x 
  If 'False _ y = y

type family Max (x :: Nat) (y :: Nat) :: Nat where 
  Max x y = If (GreaterEqual x y) x y

-- | c. Write a family to get the maximum natural in a list.
type family Maximum (xs :: [Nat]) :: Nat where 
  Maximum '[x] = x 
  Maximum '[x, y] = Max x y 
  Maximum (x ':y ': xs) = Maximum (Max x y ': xs)

{- FIVE -}

data Tree = Empty | Node Tree Nat Tree

-- | Write a type family to insert a promoted 'Nat' into a promoted 'Tree'.
type family Insert (t :: Tree) (n :: Nat) :: Tree where 
  Insert 'Empty n = 'Node 'Empty n 'Empty 
  Insert ('Node l m r) n = IfInsert (GreaterEqual n m) r l n

type family IfInsert (b :: Bool) (r :: Tree) (l :: Tree) (n :: Nat) :: Tree where 
  IfInsert 'True r _ n = Insert r n 
  IfInsert 'False _ l n = Insert l n

{- SIX -}

-- | Write a type family to /delete/ a promoted 'Nat' from a promoted 'Tree'.
type family DeleteNode (t :: Tree) (n :: Nat) :: Tree where 
  DeleteNode 'Empty _ = 'Empty 
  DeleteNode ('Node l m r) n = IfDelete (Compare n m) l r n

type family IfDelete (b :: Ordering) (l :: Tree) (r :: Tree) (n :: Nat) :: Tree where 
  IfDelete 'GT l r n = DeleteNode r n
  IfDelete 'LT l r n = DeleteNode l n 
  IfDelete 'EQ l r n = 'Empty

{- SEVEN -}

-- | With @TypeOperators@, we can use regular Haskell list syntax on the
-- type-level, which I think is /much/ tidier than anything we could define.

data HList (xs :: [Type]) where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

-- | Write a function that appends two 'HList's.
type family (+++) (xs :: [a]) (ys :: [a]) :: [a] where 
  '[] +++ ys = ys 
  (x ': xs) +++ ys = x ': (xs +++ ys)

appendHList :: HList as -> HList bs -> HList (as +++ bs)
appendHList HNil ys = ys 
appendHList (HCons x xs) ys = HCons x (appendHList xs ys)

{- EIGHT -}

-- | Type families can also be used to build up constraints. There are, at this
-- point, a couple things that are worth mentioning about constraints:
--
-- - As we saw before, '()' is the empty constraint, which simply has "no
--   effect", and is trivially solved.
--
-- - Unlike tuples, constraints are "auto-flattened": ((a, b), (c, (d, ())) is
--   exactly equivalent to (a, b, c, d). Thanks to this property, we can build
--   up constraints using type families!

type family CAppend (x :: Constraint) (y :: Constraint) :: Constraint where
  CAppend x y = (x, y)

-- | a. Write a family that takes a constraint constructor, and a type-level
-- list of types, and builds a constraint on all the types.

type family Every (c :: Type -> Constraint) (x :: [Type]) :: Constraint where
  Every _ '[] = () 
  Every c (t ': ts) = CAppend (c t) (Every c ts)

-- | b. Write a 'Show' instance for 'HList' that requires a 'Show' instance for
-- every type in the list.
instance Show (HList '[]) where 
  show HNil = "[]"
instance Show a => Show (HList '[a]) where 
  show (HCons x xs) = show x ++ show xs

-- | c. Write an 'Eq' instance for 'HList'. Then, write an 'Ord' instance.
-- Was this expected behaviour? Why did we need the constraints?
instance Eq (HList '[]) where 
  HNil == HNil = True
instance (Eq a, Eq (HList as)) => Eq (HList (a ': as)) where 
  HCons x xs == HCons y ys = x == y && xs == ys

instance Ord (HList '[]) where 
  HNil `compare` HNil = EQ
instance (Ord a, Ord (HList as)) => Ord (HList (a ': as)) where
  HCons x xs `compare` HCons y ys = x `compare` y <> xs `compare` ys

{- NINE -}

-- | a. Write a type family to calculate all natural numbers up to a given
-- input natural.

-- | b. Write a type-level prime number sieve.

-- | c. Why is this such hard work?

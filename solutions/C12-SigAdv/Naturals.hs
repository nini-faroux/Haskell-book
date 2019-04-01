module Chap12.Naturals where

data Nat = 
    Zero' 
  | Succ Nat 
  deriving (Eq, Show)

natToInteger :: Nat -> Integer 
natToInteger Zero' = 0
natToInteger (Succ Zero') = 1
natToInteger (Succ xs) = 1 + natToInteger xs

integerToNat :: Integer -> Nat 
integerToNat 0 = Zero' 
integerToNat 1 = Succ Zero' 
integerToNat n = Succ (integerToNat (n-1))

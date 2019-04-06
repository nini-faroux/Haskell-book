module C06_TypeClasses.EqInstances where

-- 1
data TisAnInteger = TisAn Integer 

instance Eq TisAnInteger where
  (TisAn x) == (TisAn y) = x == y

-- 2
data TwoIntegers = 
        Two' Integer Integer 

instance Eq TwoIntegers where
  (Two' a b) == (Two' c d) = a == c && b == d

-- 3 
data StringOrInt = 
        TisAnInt Int 
      | TisAString String 

instance Eq StringOrInt where
  (TisAnInt x) == (TisAnInt y)     = x == y
  (TisAString x) == (TisAString y) = x == y
  _ == _                           = False   

-- 4 
data Pair a = Pair a a

instance Eq a =>
        Eq (Pair a) where
  (Pair a b) == (Pair c d) = a == c && b == d

-- 5
data Tuple a b = 
        Tuple a b 

instance (Eq a, Eq b) =>
        Eq (Tuple a b) where
  (Tuple a b) == (Tuple c d) = a == c && b == d

-- 6 
data EitherOr a b = 
        Hello a
      | Goodbye b

instance (Eq a, Eq b) =>
        Eq (EitherOr a b) where
  (Hello a) == (Hello b) = a == b
  (Goodbye a) == (Goodbye b) = a == b
  _ == _ = False

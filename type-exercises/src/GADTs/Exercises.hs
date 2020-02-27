{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module GADTs.Exercises where

{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
class Countable a where 
  count :: a -> Int

instance Countable Int  where 
  count   = id
instance Countable [a]  where 
  count   = length
instance Countable Bool where 
  count x = if x then 1 else 0

-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
  Nil :: CountableList 
  Cons :: Countable a => a -> CountableList -> CountableList

-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList Nil = 0 
countList (Cons x xs) = count x + countList xs

countList' :: CountableList -> Int 
countList' = undefined -- foldr (\x xs -> if x == Nil then 0 else count x + xs) 0 

-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero Nil = Nil 
dropZero (Cons x xs) 
  | count x == 0 = dropZero xs 
  | otherwise    = Cons x (dropZero xs)

dropZero' :: CountableList -> CountableList 
dropZero' xs = undefined  -- foldr (\x xs -> if count x == 0 then xs else Cons x xs) Nil xs

-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

filterInts :: CountableList -> CountableList
filterInts = error "Contemplate me!" -- no, all we know about each element is that it's an instance of Countable

{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  AnyNil :: AnyList 
  AnyCons :: a -> AnyList -> AnyList

-- | b. How many of the following functions can we implement for an 'AnyList'?

reverseAnyList :: AnyList -> AnyList
reverseAnyList AnyNil = AnyNil
reverseAnyList (AnyCons x xs) = concatAny (reverseAnyList xs) (AnyCons x AnyNil)

concatAny :: AnyList -> AnyList -> AnyList 
concatAny AnyNil ys = ys 
concatAny (AnyCons x xs) ys = AnyCons x (concatAny xs ys) 

filterAnyList :: (a -> Bool) -> AnyList -> AnyList
filterAnyList = undefined

lengthAnyList :: AnyList -> Int
lengthAnyList AnyNil = 0 
lengthAnyList (AnyCons _ xs) = 1 + lengthAnyList xs

foldAnyList :: Monoid m => AnyList -> m
foldAnyList = undefined

isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList AnyNil = True 
isEmptyAnyList _      = False

instance Show AnyList where
  show = error "What about me?" 
  -- no, we would need a 'Show a' constraint in the GADT
  -- data AnyList where 
  --   AnyNil :: AnyList 
  --   AnyCons :: Show a => a -> AnyList -> AnyList

{- THREE -}

-- | Consider the following GADT:

data TransformableTo output where
  TransformWith :: (input -> output) -> input -> TransformableTo output

instance Show a => Show (TransformableTo a) where 
  show (TransformWith f x) = show $ f x

-- | ... and the following values of this GADT:

transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

trans3 :: TransformableTo Int 
trans3 = TransformWith (*2) 5

trans4 :: TransformableTo Bool
trans4 = TransformWith (const True) "oh"

-- | a. Which type variable is existential inside 'TransformableTo'? 
-- 'input' is existential, it only exists within the scope of the Constructor
--
-- What is the only thing we can do to it?
-- Apply the function to it 

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check?

instance Eq a => Eq (TransformableTo a) where 
  (TransformWith f x) == (TransformWith g y) = f x == g y

-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?

instance Functor TransformableTo where 
  fmap :: (outputa -> outputb) -> TransformableTo outputa -> TransformableTo outputb 
  fmap f (TransformWith g x) = TransformWith f (g x)

{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it?
eqPair :: EqPair -> Bool 
eqPair (EqPair x y) = x == y

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)

data EqPair' a where 
  EqPair' :: Eq a => a -> a -> EqPair' a

-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?

-- We still need a GADT as we can't constrain the values of the input with the Eq typeclass using 
-- a normal ADT, below we need to add "Eq a =>" to the function signature for it to work with the ADT type 

data EqPair'' a = Eq' a a 

eqPair' :: Eq a => EqPair'' a -> Bool 
eqPair' (Eq' x y) = x == y

{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox int _) = int

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox _ (IntBox n _)) = n

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers (StringBox _ (IntBox n _)) = n

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?

-- We need to be able to set return type 'a' to different concrete types, 
-- String, Int, and () 

-- removeLayer :: MysteryBox a -> MysteryBox a
-- removeLayer (IntBox _ munit) = munit 
-- removeLayer (StringBox _ mint) = mint
-- removeLayer (BoolBox _ ms) = ms 

{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)

exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!

headH :: HList (head, tail) -> head 
headH (HCons h _) = h

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

-- patternMatchMe :: HList (Int, String, Bool, ()) -> Int
-- patternMatchMe (HCons n (HCons s (HCons b HNil))) = n 

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?

append :: HList a -> HList a -> HList a
append HNil ys = ys 
-- append (HCons h1 t1) ys = HCons h1 (append t1 ys)
-- append (HCons h1 t1) (HCons h2 t2) = HCons h1 (HCons (append t1 t2) (HCons h2 HNil))

{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  Empty  :: HTree () 
  Branch :: HTree left -> centre -> HTree right -> HTree (left, centre, right)

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?

deleteLeft :: HTree (left, centre, right) -> HTree ((), centre, right) 
deleteLeft (Branch l c r) = Branch Empty c r

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. You might also need an extension or
-- two, so look out for something... flexible... in the error messages!
-- Recursion is your friend here - you shouldn't need to add a constraint to
-- the GADT!

instance Eq (HTree ()) where 
  Empty == Empty = True 

instance (Eq left, Eq right, Eq centre, Eq (HTree left), Eq (HTree centre), Eq (HTree right)) 
  => Eq (HTree (left, centre, right)) where 
    Branch l1 c1 r1 == Branch l2 c2 r2 = l1 == l2 && c1 == c2 && r1 == r2
    
{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
  -- ...

-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts = error "Implement me!"

getSeconds :: AlternatingList a b -> [b]
getSeconds = error "Implement me, too!"

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list.

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues = error "Implement me, three!"

{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool                             -> Expr Bool

-- | a. Implement the following function and marvel at the typechecker:

eval :: Expr a -> a
eval (IntValue i) = i 
eval (BoolValue b) = b 
eval (Equals e1 e2) = eval e1 == eval e2 
eval (Add e1 e2) = eval e1 + eval e2 
eval (If cond e1 e2) = if eval cond then eval e1 else eval e2

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

parse :: DirtyExpr -> Maybe (Expr Int)
parse (DirtyIntValue i) = Just $ IntValue i
parse (DirtyBoolValue b) = if b then Just $ IntValue 1 else Just $ IntValue 0 -- or Nothing
parse (DirtyEquals (DirtyIntValue e1) (DirtyIntValue e2)) = if e1 == e2 then Just $ IntValue 1 else Just $ IntValue 0 -- or Nothing
parse (DirtyAdd (DirtyIntValue e1) (DirtyIntValue e2)) = Just $ Add (IntValue e1) (IntValue e2)
parse (DirtyIf (DirtyBoolValue b) (DirtyIntValue i1) (DirtyIntValue i2)) = Just $ If (BoolValue b) (IntValue i1) (IntValue i2)

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe' in the
-- 'eval' function?

{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  -- ...

-- | b. Which types are existential?

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
composeTALs = error "Implement me, and then celebrate!"


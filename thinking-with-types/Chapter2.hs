{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

{-
  Exercise - 2.1.3-i
  If Show Int has kind CONSTRAINT, what’s the kind of Show?

  Show :: Type -> Constraint

  Ex - 2.1.3-ii
  What is the kind of Functor? 

  Functor :: (Type -> Type) -> Constraint

  Ex - 2.1.3-iii
  What is the kind of Monad? 

  Monad :: (Type -> Type) -> Constraint 

  Ex - 2.1.3-iv
  What is the kind of MonadTrans? 

  MonadTrans :: ((Type -> Type) -> Type -> Type) 
             -> Type 

-} 

{- 
  Ex - 2.4-i 
  Write a closed type family to compute Not
-}

type family Not (x :: Bool) :: Bool where 
  Not 'True = 'False 
  Not 'False = 'True

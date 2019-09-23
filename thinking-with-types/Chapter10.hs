{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{- 
  Exercise 10.1-i 
  Defunctionalize listToMaybe :: [a] -> Maybe a.
-}

listToMaybe :: [a] -> Maybe a 
listToMaybe []    = Nothing 
listToMaybe (x:_) = Just x

newtype ListToMaybe a = ListToMaybe [a]

class Eval l t | l -> t where 
  eval :: l -> t 

instance Eval (ListToMaybe a) (Maybe a) where 
  eval (ListToMaybe []) = Nothing 
  eval (ListToMaybe (x:_)) = Just x 


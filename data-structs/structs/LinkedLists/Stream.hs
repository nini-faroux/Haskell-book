module LinkedLists.Stream where 

infixr 5 :> 

data Stream a = a :> Stream a 
    deriving (Ord, Eq)

instance Functor Stream where 
  fmap f (x :> xs) = f x :> fmap f xs

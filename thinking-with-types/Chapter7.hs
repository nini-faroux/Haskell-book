{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

data HasShow where 
  HasShow :: Show t => t -> HasShow 

elimHasShow :: (forall a. Show a => a -> r) -> HasShow -> r
elimHasShow f (HasShow a) = f a

instance Show HasShow where 
  show = elimHasShow show 


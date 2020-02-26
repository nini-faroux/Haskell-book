module FlexibleInstances.Exercises where

class PopQuiz a

-- | Which of the following instances require 'FlexibleInstances'? Don't cheat
-- :D This is a tricky one, but look out for nested concrete types!

instance PopQuiz Bool
-- instance PopQuiz [Bool] - Bool isn't a type variable
instance PopQuiz [a]
instance PopQuiz (a, b)
-- instance PopQuiz [(a, b)] -- (,) ins't a type variable 
instance PopQuiz (IO a)

newtype RIO  r a = RIO (r -> IO a) -- Remember, this is a /new type/.
type    RIO' r a =      r -> IO a

-- instance PopQuiz (RIO Int a) -- Int is not a type variable 
instance PopQuiz (RIO r a)
-- instance PopQuiz (RIO' r a)
-- instance PopQuiz (r -> IO a) -- IO is not a type variable
instance PopQuiz (a -> b) -- We can write (a -> b) as ((->) a b).
-- instance PopQuiz (a -> b -> c) -- (->) is not a type variable
instance PopQuiz (a, b, c)
-- instance PopQuiz (a, (b, c)) -- (,) is not a type variable
instance PopQuiz ()
-- instance PopQuiz (a, b, c, a) -- not unique

data Pair  a = Pair  a  a
type Pair' a =      (a, a)

-- instance PopQuiz (a, a) -- not unique
instance PopQuiz (Pair a)
-- instance PopQuiz (Pair' a) -- not unique

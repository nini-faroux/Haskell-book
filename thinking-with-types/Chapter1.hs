{- Exercise - 1.2 

Determine the cardinality of: Either Bool (Bool, Maybe Bool) -> Bool

  Answer: 2^8 = 256 

  let a = Either Bool (Bool, Maybe Bool) 
  let b = Bool 
  let result = |a -> b|
             = |b|^|a|

  |a| = |Bool| + |(Bool, Maybe Bool)| 
  
  |Bool| = 2 (True | False) 
  
  |(Bool, Maybe Bool)| = |Bool| * |Maybe Bool|
          = 2 * (1 + 2) 
          = 6
  -> |a| = 2 + 6 = 8

  |b| = |Bool| = 2 
  
  -> result = |b| ^ |a| 
            = 2 ^ 8 
            = 256

-}

ex1 :: Either Bool (Bool, Maybe Bool) -> Bool
ex1 (Left b) = b
ex1 (Right (b, _)) = b

{- 
  1.4-i
  Use Curry-Howard to prove the exponent law that 
  a^b * a^c = a^(b+c) 
  That is provide functions of Type: 
  f :: (b -> a) -> (c -> a) -> Either b c -> a 
  g :: (Either b c -> a) -> (b -> a, c -> a) 
-} 

expTo :: (b -> a) -> (c -> a) -> Either b c -> a 
expTo f _ (Left x) = f x 
expTo _ g (Right y) = g y

expFrom :: (Either b c -> a) -> (b -> a, c -> a)
expFrom ef = (ef . Left, ef . Right)

{- 
  1.4-ii
  Prove (a * b)^c = a^c * b^c
-}

expTo2 :: (c -> (a, b)) -> (c -> a, c -> b)
expTo2 f = (fst . f, snd . f)

expFrom2 :: (c -> a, c -> b) -> (c -> (a, b))
expFrom2 (f, g) c = (f c, g c)

----
expTo2' :: c -> (a, b) -> (c -> a, c -> b)
expTo2' c (a, b) = (const a, const b)

{-
  1.4-iii
  Prove (a^b)^c = a^(b*c)
-}

-- uncurry
expTo3 :: (c -> b -> a) -> (b, c) -> a 
expTo3 f (b, c) = f c b

-- curry
expFrom3 :: ((b, c) -> a) -> c -> b -> a
expFrom3  f c b = f (b, c)

----
expFrom3' :: (b, c) -> a -> c -> (b -> a)
expFrom3' (b, c) a c' = const a


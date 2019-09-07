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
ex1 = undefined

module C01_LambdaCalc.LogicalOps where 

true :: a -> b -> a 
true = \x -> \_ -> x

false :: a -> b -> b
false = \_ -> \y -> y

not' :: (Bool -> Bool -> a) -> a
not' = \f -> f False True

and' :: (a -> Bool -> b) -> (Bool -> Bool -> a) -> b
and' = \f -> \g -> f (g True False) False 

or' :: (Bool -> a -> b) -> (Bool -> Bool -> a) -> b
or' = \f -> \g -> f True (g True False)

lcTest :: IO () 
lcTest = do
  putStrLn $ "not true - " ++ show (not' true)
  putStrLn $ "not false - " ++ show (not' false) 

  putStrLn $ "and false false - " ++ show (and' false false)
  putStrLn $ "and false true - " ++ show (and' false true)
  putStrLn $ "and true false - " ++ show (and' true false) 
  putStrLn $ "and true true - " ++ show (and' true true) 

  putStrLn $ "or false false - " ++ show (or' false false) 
  putStrLn $ "or false true - " ++ show (or' false true)
  putStrLn $ "or true false - " ++ show (or' true false) 
  putStrLn $ "or true true - " ++ show (or' true true) 




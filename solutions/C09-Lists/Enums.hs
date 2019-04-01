module Chap9.Enums where

eftInt :: Int -> Int -> [Int] 
eftInt x y 
  | y < x = goPred x y 
  | otherwise = goSucc x y 
  where 
    goSucc x y 
      | x == y = [y] 
      | otherwise = goSucc (succ x) y
    goPred x y 
      | x == y = [y]
      | otherwise = x : goPred (pred x) y

eftChar :: Char -> Char -> [Char] 
eftChar x y 
  | y < x  = goPred x y 
  | otherwise = goSucc x y 
  where
    goSucc x y
      | x == y = [y]
      | otherwise = x : goSucc (succ x) y
    goPred x y 
      | x == y = [y]
      | otherwise = x : goPred (pred x) y

eftOrd :: Ordering -> Ordering -> [Ordering] 
eftOrd x y 
  | compare x y == GT = goPred x y 
  | otherwise = goSucc x y 
  where 
    goSucc x y 
      | x == y = [y] 
      | otherwise = goSucc (succ x) y
    goPred x y 
      | x == y = [y]
      | otherwise = goPred (pred x) y

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False _     = [False, True]
eftBool True True   = [True]
eftBool True _      = [] 

module C23_State.Rolls where

import Control.Applicative (liftA3) 
import Control.Monad (replicateM) 
import Control.Monad.Trans.State 
import System.Random

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

type Count = Int 
type Target = Int 
type Sum' = Int 

-- 1
rollsToGetN :: Target -> StdGen -> Int 
rollsToGetN n g = go 0 0 n g 
  where 
    go :: Sum' -> Count -> Target -> StdGen -> Int 
    go sum count target gen 
      | sum >= target = count 
      | otherwise = let (die, nextGen) = randomR (1, 6) gen
                    in go (sum + die) (count + 1) target nextGen

-- 2
rollsCountLogged :: Target -> StdGen -> (Int, [Die]) 
rollsCountLogged t g = go 0 0 t g (0, [])
  where
    go :: Sum' -> Count -> Target -> StdGen -> (Int, [Die]) -> (Int, [Die])
    go sum count target gen acc 
      | sum >= target = acc 
      | otherwise = let (die, nextGen) = randomR (1, 6) gen 
                    in go (sum + die) (count + 1) target nextGen (fst acc + 1,  snd acc ++ [intToDie die])

intToDie :: Int -> Die 
intToDie n = 
    case n of
      1 -> DieOne
      2 -> DieTwo
      3 -> DieThree 
      4 -> DieFour 
      5 -> DieFive 
      6 -> DieSix
      -- Use 'error'
      -- _extremely_ sparingly. 
      x -> error $ "intToDie got non 1-6 integer: " ++ show x

module C22_Reader.WarmUp where

import Data.Char
import Control.Applicative

cap :: String -> String
cap = map toUpper 

rev :: String -> String
rev = reverse 

-- 1
composed :: String -> String
composed = rev . cap

fmapped :: String -> String
fmapped = fmap rev cap

-- 2
tupledAp :: String -> (String, String)
tupledAp = (,) <$> cap <*> rev

tupled' :: String -> (String, String)
tupled' = liftA2 (,) cap rev

-- 3 
tupledMon :: String -> (String, String)
tupledMon = do
  a <- cap
  b <- rev 
  return (a, b)

tupledMon' :: String -> (String, String)
tupledMon' xs = return xs >>= \x -> (,) (cap x) (rev x)

nonMon :: String -> (String, String)
nonMon x = (,) (cap x) (rev x) 

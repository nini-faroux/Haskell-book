module Chap11.VigenereCipher where

import Data.Char (ord, chr, isUpper)
import Data.Maybe (fromJust)

type Keyword = String
type Message = String
type Encoded = String

-- vigenere "ALLY" "MEET AT DAWN" 
-- "MPPR AE OYWY"
caesar' :: Int -> Char -> Char
caesar' n c
  | c == ' ' = ' '
  | isUpper c = chr $ (((ord c - 65) + n) `mod` 26) + 65
  | otherwise = chr $ (((ord c - 97) + n) `mod` 26) + 97

keys :: [(Char, Int)]
keys = zipWith (,) ['A'..'Z'] [0..25]

vigenere :: Keyword -> Message -> Encoded
vigenere kw mes = go kw mes []
  where
    go _ [] acc = reverse acc
    go [] m acc = go kw m acc
    go kks@(k:ks) (m:ms) acc
      | m == ' '  = go kks ms (' ' : acc)
      | otherwise = go ks ms ((caesar' (fromJust $ lookup k keys) m) : acc)

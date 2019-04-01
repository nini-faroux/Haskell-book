module Chap24.ParserOne where

import Text.Trifecta

-- 1
oneEof :: Parser ()
oneEof = char '1' >> eof

oneTwoEof :: Parser () 
oneTwoEof = char '1' >> char '2' >> eof

-- 2 
p123 :: String -> Result String
p123 input 
  | input == "1"   = parseString (string "1") mempty input
  | input == "12"  = parseString (string "12") mempty input 
  | input == "123" = parseString (string "123") mempty input
  | otherwise      = return "No parse"

-- 3
string' :: (Monad m, CharParsing m) => String -> m String 
string' = foldr (\x y -> char x >>= \a -> (:) a <$> y) (return []) 

-- or 
string'' :: (Monad m, CharParsing m) => String -> m String 
string'' input = go input [] 
  where
    go [] acc     = return acc 
    go (x:xs) acc = char x >>= \x -> go xs (acc ++ [x])

----
eofParse :: Parser a -> Result a
eofParse p = parseString p mempty "1234"

mainParse :: IO ()
mainParse = do
    print $ parseString (string' "abc") mempty "abcd"
    print $ parseString (string' "abc") mempty "arbcd"
    print $ parseString (string'' "abc") mempty "abcd" 
    print $ p123 "1"
    print $ p123 "12"
    print $ p123 "123"
    print $ eofParse oneEof
    print $ eofParse oneTwoEof

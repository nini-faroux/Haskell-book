module Chap24.ParserOne where

import Text.Trifecta

-- 1
eofParse :: Parser a -> Result a
eofParse p = parseString p mempty "1234"

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
string' input = go input [] 
  where
    go [] acc     = return acc 
    go (x:xs) acc = char x >>= \x -> go xs (acc ++ [x])

pnl :: String -> IO ()
pnl s = putStrLn ('\n' : s)

mainParse :: IO ()
mainParse = do
    pnl "one-eof:"
    print $ eofParse oneEof
    pnl "two-eof:"
    print $ eofParse oneTwoEof

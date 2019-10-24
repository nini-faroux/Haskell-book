module PCPExamples.C07_MVar.PhoneBook where 

import qualified Data.Map as Map
import Control.Concurrent

type Name = String 
type PhoneNumber = String 
type PhoneBook = Map.Map Name PhoneNumber 

newtype PhoneBookState = PhoneBookState (MVar PhoneBook) 

new :: IO PhoneBookState 
new = do 
  m <- newMVar Map.empty 
  return $ PhoneBookState m

insert :: PhoneBookState -> Name -> PhoneNumber -> IO () 
insert (PhoneBookState m) name number = do 
  book <- takeMVar m 
  let book' = Map.insert name number book 
  putMVar m book' 
  seq book' (return ())

bookLookup :: PhoneBookState -> Name -> IO (Maybe PhoneNumber) 
bookLookup (PhoneBookState m) name = do 
  book <- takeMVar m 
  putMVar m book 
  return $ Map.lookup name book

phoneMain :: IO () 
phoneMain = do 
  s <- new 
  sequence_ [ insert s ("name" ++ show n) (show n) | n <- [1..10000]]
  bookLookup s "name999" >>= print 
  bookLookup s "unknown" >>= print 

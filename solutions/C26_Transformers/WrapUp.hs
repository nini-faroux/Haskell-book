module C26_Transformers.WrapUp where

import Control.Monad.Trans.Except 
import Control.Monad.Trans.Maybe 
import Control.Monad.Trans.Reader 

-- 1
getEmbedded :: MaybeT (ExceptT String (ReaderT () IO)) Int 
getEmbedded = MaybeT . ExceptT . ReaderT $ \_ -> return $ fge ()

fge :: b -> Either a (Maybe Int)
fge = const . Right . Just $ 1

-------------------------------------------------------
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int 
embedded = return 1 

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded 

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int)) 
eitherUnwrap = runExceptT maybeUnwrap 

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

unwrap :: IO (Either String (Maybe Int)) 
unwrap = readerUnwrap ()

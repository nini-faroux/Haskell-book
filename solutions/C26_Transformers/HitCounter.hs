{-# LANGUAGE OverloadedStrings #-}

module C26_Transformers.HitCounter where 

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader 
import Data.IORef 
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import System.Environment (getArgs)
import Web.Scotty.Trans 

import qualified Data.Text.Lazy as TL 
import qualified Data.Map as M 

data Config = 
    Config {
      counts :: IORef (M.Map Text Integer)
    , prefix :: Text 
    }

type Scotty = ScottyT Text (ReaderT Config IO) 

type Handler = ActionT Text (ReaderT Config IO)

clickCount :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
clickCount k m 
  | M.lookup k m == Nothing = (M.insert k 1 m, 0) 
  | otherwise               = (M.insert k ((fromMaybe 0 $ M.lookup k m) + 1) m, fromMaybe 0 $ M.lookup k m)

        {--
app :: Scotty () 
app = 
  get "/:key" $ do
    unprefixed <- param "key"
    let key' = mappend undefined unprefixed 
    newInteger <- undefined 
    html $ mconcat ["<h1>Success! Count was: ", TL.pack $ show newInteger, "</h1>"]

mainScott :: IO () 
mainScott = do
  [prefixArg] <- getArgs 
  counter <- newIORef M.empty 
  let config = undefined 
      runr = undefined 
  scottyT 3000 runR app --}

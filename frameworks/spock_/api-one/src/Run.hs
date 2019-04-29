{-# LANGUAGE OverloadedStrings #-}

module Run (runApp, app) where 

import           Web.Spock
import           Web.Spock.Config
import           Network.Wai (Middleware)

import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Database.Persist.Sqlite

import           Api (api)
import           Models
import           Errors (handler')

runApp :: IO ()
runApp = runSpock 8080 app

app :: IO Middleware 
app = do
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg <- defaultSpockCfg () (PCPool pool) () 
  let cfg = spockCfg {spc_errorHandler = handler'}
  runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool 
  spock cfg api

  

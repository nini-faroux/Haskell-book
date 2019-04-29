{-# LANGUAGE OverloadedStrings #-}

module Run (runApp, app) where 

import           Web.Spock (spock, runSpock)
import           Web.Spock.Config (PoolOrConn(PCPool), defaultSpockCfg, spc_errorHandler) 
import           Network.Wai (Middleware)

import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Database.Persist.Sqlite (createSqlitePool, runSqlPool, runMigration)

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

  

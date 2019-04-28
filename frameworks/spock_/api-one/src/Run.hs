{-# LANGUAGE OverloadedStrings #-}

module Run where 

import           Web.Spock
import           Web.Spock.Config

import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Database.Persist.Sqlite

import           Api
import           Models
import           Errors

runApp :: IO ()
runApp = do
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg <- defaultSpockCfg () (PCPool pool) ()
  let cfg = spockCfg {spc_errorHandler = handler'}
  runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
  runSpock 8080 (spock cfg app)

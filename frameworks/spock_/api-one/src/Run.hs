{-# LANGUAGE OverloadedStrings #-}

module Run (runApp, app) where

import           Network.Wai             (Middleware)
import           Web.Spock               (runSpock, spock)
import           Web.Spock.Config        (PoolOrConn (PCPool), defaultSpockCfg,
                                          spc_errorHandler)

import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Database.Persist.Sqlite (createSqlitePool, runMigration,
                                          runSqlPool)

import           Api                     (api)
import           Errors                  (handler')
import           Models

runApp :: IO ()
runApp = runSpock 8080 app

app :: IO Middleware
app = do
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg <- defaultSpockCfg () (PCPool pool) ()
  let cfg = spockCfg {spc_errorHandler = handler'}
  runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
  spock cfg api



{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Models where 

import           Web.Spock

import           Database.Persist.Sqlite hiding (get, delete)
import           Database.Persist.TH

import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Data.Text               (Text)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json 
  name Text 
  age Int
  deriving Show
|]

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) 
       => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

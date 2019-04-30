{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models  where

import           Web.Spock               (HasSpock, SpockConn, runQuery)

import           Database.Persist.Sqlite hiding (delete, get)
import           Database.Persist.TH     (mkMigrate, mkPersist,
                                          persistLowerCase, share, sqlSettings)

import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Data.Text               (Text)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  name Text
  age Int
  deriving Show
|]

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

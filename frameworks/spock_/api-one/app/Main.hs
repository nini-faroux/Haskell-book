{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANUGAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson              hiding (json)
import           Data.Monoid             ((<>))
import           Data.Text               (Text, pack)
import           GHC.Generics

import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Database.Persist        hiding (get)
import qualified Database.Persist        as P
import           Database.Persist.Sqlite hiding (get)
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json 
  name Text 
  age Int
  deriving Show
|]

type Api = SpockM SqlBackend () () ()
type ApiAction a = SpockAction SqlBackend () () a

app :: Api
app = do
  getPerson
  postPerson

getPerson :: SpockCtxM ctx conn sess st ()
getPerson =
  get "people" $
    json 
      [Person { personName = "nini", personAge = 500 }, 
       Person { personName = "major", personAge = 432}
      ]

postPerson :: SpockCtxM () SqlBackend () () ()
postPerson =
  post "people" $ do
    pers <- jsonBody' :: ApiAction Person
    text $ "Parsed: " <> pack (show pers)

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg <- defaultSpockCfg () (PCPool pool) ()
  runSpock 8080 (spock spockCfg app)



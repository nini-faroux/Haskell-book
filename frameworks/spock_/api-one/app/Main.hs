{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
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
import           Network.HTTP.Types

import           Data.Aeson              hiding (json)
import           Data.Monoid             ((<>))
import           Data.Text               (Text, pack)
import           GHC.Generics

import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Database.Persist        hiding (get, delete)
import qualified Database.Persist        as P
import           Database.Persist.Sqlite hiding (get, delete)
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json 
  name Text 
  age Int
  deriving Show
|]

type Api = SpockM SqlBackend () () ()
type ApiAction a = SpockAction SqlBackend () () a

app :: Api
app = do
  getUsers
  getUser
  postUser
  deleteUser

getUsers :: SpockCtxM ctx SqlBackend sess st ()
getUsers =
  get "users" $ do
    users <- runSQL $ selectList [] [Asc UserId]
    json users

getUser :: SpockCtxM () SqlBackend () () () 
getUser =
  get ("users" <//> var) $ \userId -> do
    mu <- runSQL $ P.get userId :: ApiAction (Maybe User)
    case mu of
      Nothing -> setStatus status404 >> errorJson 2 "User not found"
      Just user  -> setStatus status200 >> json user

deleteUser :: SpockCtxM () SqlBackend () () ()
deleteUser = 
  delete ("users" <//> var) $ \userId -> do
    mu <- runSQL $ P.get userId :: ApiAction (Maybe User)
    case mu of
      Nothing -> setStatus status404 >> errorJson 2 "User not found"
      Just _  -> do 
        runSQL $ P.delete (userId :: UserId)
        setStatus status204 

postUser :: SpockCtxM () SqlBackend () () ()
postUser =
  post "users" $ do
    mu <- jsonBody :: ApiAction (Maybe User)
    case mu of 
      Nothing -> setStatus status400 >> errorJson 1 "Failed to parse request body as User"
      Just user -> do
        id <- runSQL $ insert user 
        setStatus status201 
        json $ object ["result" .= String "success", "id" .= id]

errorJson :: Int -> Text -> ApiAction () 
errorJson code message = 
  json $ 
    object 
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]

runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) 
       => SqlPersistT (LoggingT IO) a -> m a
runSQL action = runQuery $ \conn -> runStdoutLoggingT $ runSqlConn action conn

main :: IO ()
main = do
  pool <- runStdoutLoggingT $ createSqlitePool "api.db" 5
  spockCfg <- defaultSpockCfg () (PCPool pool) ()
  runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool
  runSpock 8080 (spock spockCfg app)

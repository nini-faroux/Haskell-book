{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module WYExercises where 

import Control.Lens
import Data.Text

data User = 
  User {
    _name :: Text 
  , _userId :: Int 
  , _metaData :: UserInfo
  } deriving Show 

data UserInfo = 
  UserInfo { 
    _numLogins :: Int 
  , _associatedIPs :: [Text] 
  } deriving (Show) 

makeLenses ''User 
makeLenses ''UserInfo

user1 :: User
user1 = User
  { _name = "qiao.yifan"
  , _userId = 103
  , _metaData = UserInfo
    { _numLogins = 20
    , _associatedIPs =
      [ "52.39.193.61"
      , "52.39.193.75"
      ]
    }
  }

viewName :: User -> Text 
viewName u = u ^. name

viewNumLogins :: User -> Int 
viewNumLogins u = u ^. metaData.numLogins

setNumLogins :: User -> Int -> User 
setNumLogins u n = u & metaData.numLogins .~ n

addIP :: User -> Text -> User 
addIP u ip = u & metaData.associatedIPs %~ (ip :)

incrementNumLogins :: User -> User 
incrementNumLogins = metaData.numLogins %~ (+ 1) 

setUserInfo :: User -> UserInfo -> User 
setUserInfo u info = u & metaData .~ info

setUserId :: User -> Int -> User 
setUserId u n = u & userId .~ n

setIPs :: User -> [Text] -> User 
setIPs u ips = u & metaData.associatedIPs .~ ips

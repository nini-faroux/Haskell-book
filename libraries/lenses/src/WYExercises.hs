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

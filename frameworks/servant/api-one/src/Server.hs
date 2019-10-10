{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Server where 

import Servant 
import Data.Aeson (ToJSON)
import GHC.Generics
import Data.Time.Calendar

type UserAPI1 = "users" :> Get '[JSON] [User]

data User = User {
    name :: String 
  , age :: Int 
  , email :: String 
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User

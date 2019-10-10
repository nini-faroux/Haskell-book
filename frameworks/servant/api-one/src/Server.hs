{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Server where 

import Servant 
import Data.Aeson (ToJSON)
import GHC.Generics
import Data.Time.Calendar
import Network.Wai 
import Network.Wai.Handler.Warp (run)

type UserAPI1 = "users" :> Get '[JSON] [User]

data User = User {
    name :: String 
  , age :: Int 
  , email :: String 
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

type UserAPI2 = 
       "users" :> Get '[JSON] [User] 
  :<|> "albert" :> Get '[JSON] User 
  :<|> "isaac" :> Get '[JSON] User

instance ToJSON User

users1 :: [User]
users1 =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]

isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

users2 :: [User]
users2 = [isaac, albert]

server1 :: Server UserAPI1 
server1 = return users1

server2 :: Server UserAPI2 
server2 = 
       return users2 
  :<|> return albert 
  :<|> return isaac
  
app1 :: Application 
app1 = serve (Proxy @UserAPI1) server1

app2 :: Application 
app2 = serve (Proxy @UserAPI2) server2

mainServer :: IO () 
mainServer = run 8081 app2

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Root where 

import           Web.Spock
import           Data.Aeson hiding (json)
import           Database.Persist.Sqlite hiding (delete, get)

getRoot :: SpockCtxM ctx SqlBackend sess st () 
getRoot = get root $ json $ object [ "root" .= String "users"]
    

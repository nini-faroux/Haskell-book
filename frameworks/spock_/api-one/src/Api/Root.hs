{-# LANGUAGE QuasiQuotes #-}

module Api.Root where 

import           Web.Spock
import           Database.Persist.Sqlite hiding (delete, get)

import           Text.RawString.QQ (r)

getRoot :: SpockCtxM ctx SqlBackend sess st () 
getRoot =
  get root $ json [r|Welcome - visit - /users - for list of users|]

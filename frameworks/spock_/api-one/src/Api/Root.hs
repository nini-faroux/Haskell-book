{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Api.Root where

import           Data.Aeson              hiding (json)
import           Database.Persist.Sqlite hiding (delete, get)
import           Web.Spock

getRoot :: SpockCtxM ctx SqlBackend sess st ()
getRoot = get root $ json $ object [ "root" .= String "users"]


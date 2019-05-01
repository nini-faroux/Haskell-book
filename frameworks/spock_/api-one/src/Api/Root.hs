{-# LANGUAGE OverloadedStrings #-}

module Api.Root where

import           Data.Aeson              (Value (String), object, (.=))
import           Database.Persist.Sqlite (SqlBackend)
import           Web.Spock               (SpockCtxM, get, json, root)

getRoot :: SpockCtxM ctx SqlBackend sess st ()
getRoot = get root $ json $ object ["route" .= String "users"]


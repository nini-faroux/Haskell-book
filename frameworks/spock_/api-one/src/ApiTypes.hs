module ApiTypes (Api, ApiAction) where

import           Database.Persist.Sqlite (SqlBackend)
import           Web.Spock               (SpockAction, SpockM)

type Api = SpockM SqlBackend () () ()
type ApiAction a = SpockAction SqlBackend () () a

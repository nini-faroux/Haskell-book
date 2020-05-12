{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module WYPrisms where 

import WYLenses ((%~), (.~))
import Control.Lens hiding ((^?), (%~), (.~))
import Data.Aeson (Value(..))
import Data.Aeson.QQ
import Data.Aeson.Lens (values, key, _String, _Integer, AsValue(..))
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Monoid (First(..), getFirst)

user3 :: Value
user3 = [aesonQQ|
  {
    "name": "qiao.yifan",
    "email": "qyifan@xingxin.com"
  }
|]

user4 :: Value
user4 = [aesonQQ|
  {
    "name": "ye.xiu",
    "metadata": {
      "num_logins": 27
    }
  }
|]

infixl 8 ^?
(^?) :: s -> ((a -> Const (First a) b) -> s -> Const (First a) t) -> Maybe a 
(^?) s f = getFirst . getConst $ f (Const . First . Just) s

_Just :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
_Just = traverse

getNumLogins :: AsValue s => s -> Maybe Integer
getNumLogins u = u ^? key "metadata".key "num_logins"._Integer

getEmail :: AsValue s => s -> Maybe T.Text
getEmail u = u ^? key "email"._String

viewEmail :: AsValue s => s -> T.Text 
viewEmail u = u ^. key "email"._String

getIPs :: AsValue s => s -> Maybe (V.Vector Value)
getIPs u = u ^? key "metadata".key "associatedIPs"._Array

upperName :: AsValue s => s -> Maybe T.Text 
upperName u = u ^? key "name"._String.to T.toUpper

setName :: Value -> T.Text -> Value 
setName u nm = u & key "name"._String .~ nm

reverseName :: Value -> Value 
reverseName u = u & key "name"._String %~ T.reverse

setAndReverseName :: Value -> T.Text -> Value 
setAndReverseName u = reverseName . setName u

incrementLogins :: Value -> Value 
incrementLogins u = u & key "metadata".key "num_logins"._Integer %~ (+ 1)

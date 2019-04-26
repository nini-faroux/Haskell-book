{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main where

import Web.Spock
import Web.Spock.Config 

import Data.Aeson hiding (json)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import GHC.Generics 

data Person = Person 
  { name :: Text
  , age :: Int 
  } deriving (Generic, Show)
    
instance ToJSON Person 
instance FromJSON Person

type Api = SpockM () () () ()
type ApiAction a = SpockAction () () () a

app :: Api 
app = do
  getPerson
  postPerson

getPerson :: SpockCtxM ctx conn sess st ()
getPerson = 
  get "people" $ 
    json [Person { name = "nini", age = 500 }, Person { name = "major", age = 432}]

postPerson :: SpockCtxM () () () () ()
postPerson = 
  post "people" $ do
    pers <- jsonBody' :: ApiAction Person 
    text $ "Parsed: " <> pack (show pers)

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase () 
  runSpock 8080 (spock spockCfg app)



{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Problems.Exercise where

import Lens.Micro.Platform 
import Data.Text (Text) 
import Test.Hspec

data Address = 
  Address { 
    _street :: !Text 
  , _city :: !Text 
  }

makeLenses ''Address 

data Person =
  Person {
    _name :: !Text 
  , _address :: !Address 
  , _age :: !Int 
  } 

makeLenses ''Person 

hollywood :: Text 
hollywood = "Hollywood Blvd"

alice :: Person 
alice = Person 
  { _name = "Alice" 
  , _address = Address { _street = hollywood, _city = "Los Angeles"} 
  , _age = 30
  } 

wilshire :: Text 
wilshire = "Wilshire Blvd" 

aliceWilshire :: Person 
aliceWilshire = undefined 

getStreet :: Person -> Text 
getStreet = undefined

-- increase age by 1 
birthday :: Person -> Person 
birthday = undefined 

getAge :: Person -> Int 
getAge = undefined 

mainEx1 :: IO () 
mainEx1 = hspec $ do
  it "lives on Wilshire" $ 
    _street (_address aliceWilshire) `shouldBe` wilshire 
  it "getStreet works" $ 
    getStreet alice `shouldBe` hollywood 
  it "birthday" $ 
    getAge (birthday alice) `shouldBe` _age alice + 1
           


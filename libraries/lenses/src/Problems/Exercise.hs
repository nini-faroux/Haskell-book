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

wilshire :: Text 
wilshire = "Wilshire Blvd" 

someStreet :: Text 
someStreet = "Some Street"

alice :: Person 
alice = Person 
  { _name = "Alice" 
  , _address = Address { _street = hollywood, _city = "Los Angeles"} 
  , _age = 30
  } 

-- set Alice's street to Wilshire
aliceWilshire :: Person 
aliceWilshire = setStreet wilshire alice 

aliceSomeStreet :: Person 
aliceSomeStreet = setStreet' someStreet alice 

setStreet :: Text -> Person -> Person 
setStreet st = over (address . street) (const st) 

setStreet' :: Text -> Person -> Person 
setStreet' = set (address . street) 

getStreet :: Person -> Text 
getStreet person = person^.address.street

-- increase age by 1 
birthday :: Person -> Person 
birthday person = set age (getAge person + 1) person 

getAge :: Person -> Int 
getAge = view age 

getAge' :: Person -> Int 
getAge' person = person^.age 

mainEx1 :: IO () 
mainEx1 = hspec $ do
  it "lives on Wilshire" $ 
    _street (_address aliceWilshire) `shouldBe` wilshire 
  it "lives on some street" $ 
    _street (_address aliceSomeStreet) `shouldBe` someStreet 
  it "getStreet works" $ 
    getStreet alice `shouldBe` hollywood 
  it "birthday" $ 
    getAge (birthday alice) `shouldBe` _age alice + 1

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

data Person =
  Person {
    _name :: !Text 
  , _address :: !Address 
  , _age :: !Int 
  } 

makeLenses ''Person 
makeLenses ''Address 

streetL :: Lens' Address Text 
streetL = lens _street (\addr newSt -> addr { _street = newSt })

cityL :: Lens' Address Text 
cityL = lens _city (\addr newCity -> addr { _city = newCity })

ageL :: Lens' Person Int 
ageL = lens _age (\pers newAge -> pers { _age = newAge })

addressL :: Lens' Person Address 
addressL = lens _address (\pers newAddr -> pers { _address = newAddr })

personStreetL :: Lens' Person Text 
personStreetL = addressL . streetL 

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

aliceWilshireManual :: Person 
aliceWilshireManual = setStreetManual wilshire alice 

aliceSomeStreet :: Person 
aliceSomeStreet = setStreet' someStreet alice 

setStreet :: Text -> Person -> Person 
setStreet st = over (address . street) (const st) 

setStreet' :: Text -> Person -> Person 
setStreet' = set (address . street) 

setStreetManual :: Text -> Person -> Person 
setStreetManual = set personStreetL 

getStreet :: Person -> Text 
getStreet person = person^.address.street

getStreetManual :: Person -> Text 
getStreetManual person = person^.addressL.streetL 

-- increase age by 1 
birthday :: Person -> Person 
birthday person = set age (getAge person + 1) person 

birthdayManual :: Person -> Person 
birthdayManual person = set ageL (getAgeManual person + 1) person

getAge :: Person -> Int 
getAge = view age 

getAge' :: Person -> Int 
getAge' person = person^.age 

getAgeManual :: Person -> Int 
getAgeManual person = person^.ageL 

mainEx1 :: IO () 
mainEx1 = hspec $ do
  -- exercise 1 tests 
  it "lives on Wilshire" $ 
    _street (_address aliceWilshire) `shouldBe` wilshire 
  it "lives on some street" $ 
    _street (_address aliceSomeStreet) `shouldBe` someStreet 
  it "getStreet works" $ 
    getStreet alice `shouldBe` hollywood 
  it "birthday" $ 
    getAge (birthday alice) `shouldBe` _age alice + 1
  -- exercise 2 tests, manual lenses 
  it "manual lives on wilshire" $ 
    _street (_address aliceWilshireManual) `shouldBe` wilshire 
  it "manual getStreet works" $ 
    getStreetManual alice `shouldBe` hollywood 
  it "manual birthday" $ 
    getAgeManual (birthdayManual alice) `shouldBe` _age alice + 1

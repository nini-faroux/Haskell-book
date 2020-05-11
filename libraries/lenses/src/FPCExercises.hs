{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module FPCExercises () where

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

{- test data -}

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

-- templatehaskell 
makeLenses ''Person 
makeLenses ''Address 

{- make the lenses without the lens helper function -}

-- streetLF :: Functor f => (Text -> f Text) -> Address -> f Address 
streetLF :: Lens' Address Text 
streetLF f address = (\street -> address { _street = street}) <$> f (_street address)

addressLF :: Lens' Person Address 
addressLF f person = (\address -> person { _address = address }) <$> f (_address person)

ageLF :: Lens' Person Int 
ageLF f person = (\age -> person { _age = age }) <$> f (_age person)

{- make the lenses using the lens helper function -}

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

personStreetLF :: Lens' Person Text 
personStreetLF = addressLF . streetLF

{- use the lenses -}

-- use template generated 
getAge :: Person -> Int 
getAge = view age 

getAge' :: Person -> Int 
getAge' person = person^.age 

-- use manual lens func created 
getAgeManual :: Person -> Int 
getAgeManual person = person^.ageL 

-- use manual functor created 
getAgeMF :: Person -> Int 
getAgeMF person = person^.ageLF

getStreet :: Person -> Text 
getStreet person = person^.address.street

getStreetManual :: Person -> Text 
getStreetManual person = person^.addressL.streetL 

getStreetMF :: Person -> Text 
getStreetMF person = person^.addressLF.streetLF 

setStreet :: Text -> Person -> Person 
setStreet st = over (address . street) (const st) 

setStreet' :: Text -> Person -> Person 
setStreet' = set (address . street) 

setStreetManual :: Text -> Person -> Person 
setStreetManual = set personStreetL 

setStreetMF :: Text -> Person -> Person 
setStreetMF = set personStreetLF 

birthday :: Person -> Person 
birthday = over age (+1) 

birthdayManual :: Person -> Person 
birthdayManual = over ageL (+1) 

birthdayMF :: Person -> Person 
birthdayMF = over ageLF (+1) 

{- use on the test data -}

aliceWilshire :: Person 
aliceWilshire = setStreet wilshire alice 

aliceWilshireManual :: Person 
aliceWilshireManual = setStreetManual wilshire alice 

aliceWilshireMF :: Person 
aliceWilshireMF = setStreetMF wilshire alice 

aliceSomeStreet :: Person 
aliceSomeStreet = setStreet' someStreet alice 

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

  -- exercise 3 tests, without lens helper 
  it "manual no helper lives on wilshire" $ 
    _street (_address aliceWilshireMF) `shouldBe` wilshire 
  it "manual no helper getStreet works" $ 
    getStreetMF alice `shouldBe` hollywood 
  it "manual no helper birthday" $ 
    getAgeManual (birthdayMF alice) `shouldBe` _age alice + 1
    
  -- exercise 4, tuple test  
  it "fun with tuples" $
    let tupleLens = _2 . _1 
        tuple :: ((Int, Double), (Bool, Char, String))
        tuple = ((1, 2), (True, 'x', "Hello World"))
     in over tupleLens not tuple `shouldBe`
            ((1, 2), (False, 'x', "Hello World"))

  -- exercise 5, prisms 
  it "over left on left" $
    let val :: Either Int Double
        val = Left 5
     in over _Left (+ 1) val `shouldBe` Left 6
  it "over left on right" $
    let val :: Either Int Double
        val = Right 5
     in over _Left (+ 1) val `shouldBe` Right 5
  it "set left on left" $
    let val :: Either Int Double
        val = Left 5
     in set _Left 6 val `shouldBe` Left 6
  it "set left on right" $
    let val :: Either Int Double
        val = Right 5
     in set _Left 6 val `shouldBe` Right 5

{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Wai
import Web.Spock (spockAsApp)

import Run (app)

main :: IO ()
main = hspec spec

spec :: Spec 
spec = 
  with (spockAsApp app) $ do
    describe "GET /" $ 
      it "is the root route" $
        get "/" `shouldRespondWith` "Welcome - visit - /users - for list of users"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Web.Spock           (spockAsApp)

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import           Run                 (app)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  with (spockAsApp app) $ do
    describe "GET /" $
      it "root GET request" $
        get "/" `shouldRespondWith` [json|{"root": "users"}|] {matchStatus=200}


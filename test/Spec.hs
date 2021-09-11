{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import qualified Data.Map as Map

import App.Config
import App.Request
import Type.Response

main :: IO ()
main = hspec test

listUsers :: Map.Map Int Int
listUsers = Map.empty

token :: Token
token = getToken conf 

conf :: ConfData
conf = ConfData "helpText" "repeatText" 3 "buttonJson" (Token "12345token")

responseText :: ResponseAll
responseText = RT $ RespText  123 1234 "message"

responseDocument :: ResponseAll
responseDocument = RD $ RespDocument 234 2345 "fileId234"

responseButton :: ResponseAll
responseButton = RB $ RespButton 345 3456 "1"

responseEmpty :: ResponseAll
responseEmpty = RE $ RespEmpty ()

responsePhoto :: ResponseAll
responsePhoto = RP $ RespPhoto 456 4567 "fileId456"

test :: Spec
test = do
  describe "Test function addNewUser : " $ do
    it "test - add new user (Text)" $ do
      let result = addNewUser responseText listUsers (startRepeat conf) 
      result `shouldBe` Map.singleton 1234 3 
    it "test - add new user (Document)" $ do
      let result = addNewUser responseDocument listUsers (startRepeat conf) 
      result `shouldBe` Map.singleton 2345 3 
    it "test - add new user (Button)" $ do
      let result = addNewUser responseButton listUsers (startRepeat conf) 
      result `shouldBe` Map.singleton 3456 1
    it "test - add new user (Empty)" $ do
      let result = addNewUser responseEmpty listUsers (startRepeat conf) 
      result `shouldBe` Map.empty 
  describe "Test function getOffset : " $ do
    it "test - get offset (Text)" $ do
      let result = getOffset responseText
      result `shouldBe` "124"
    it "test - get offset (Photo)" $ do
      let result = getOffset responsePhoto
      result `shouldBe` "457"
    it "test - get offset (Empty)" $ do
      let result = getOffset responseEmpty
      result `shouldBe` " "

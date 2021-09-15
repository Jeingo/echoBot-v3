{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import qualified Data.Map as Map
import Data.Functor.Identity

import App.Config
import qualified App.Handle.Request as Handle
import Type.Response

main :: IO ()
main = hspec test

listUsers :: Map.Map Int Int
listUsers = Map.empty

token :: Token
token = getToken conf 

conf :: ConfData
conf = ConfData "helpText" "repeatText" 3 "buttonJson" (Token "12345token") DEBUG

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

logH :: Handle.LogHandle Identity
logH = Handle.LogHandle { Handle.logger = \level typeLog mess -> return () }

logLevel :: LogLevel
logLevel = DEBUG

addNewUser :: Handle.LogHandle Identity -> LogLevel -> ResponseAll -> ListUsers -> Int -> Identity ListUsers
addNewUser logH logLevel response listUsers numOfRepeat = Handle.addNewUser logH logLevel response listUsers numOfRepeat

getOffset :: Handle.LogHandle Identity -> LogLevel -> ResponseAll -> Identity Offset
getOffset logH logLevel resp = Handle.getOffset logH logLevel resp

test :: Spec
test = do
  describe "Test function addNewUser : " $ do
    it "test - add new user (Text)" $ do
      let result = addNewUser logH logLevel responseText listUsers (startRepeat conf) 
      result `shouldBe` return (Map.singleton 1234 3 )
    it "test - add new user (Document)" $ do
      let result = addNewUser logH logLevel responseDocument listUsers (startRepeat conf) 
      result `shouldBe` return (Map.singleton 2345 3 ) 
    it "test - add new user (Button)" $ do
      let result = addNewUser logH logLevel responseButton listUsers (startRepeat conf) 
      result `shouldBe` return (Map.singleton 3456 1 )
    it "test - add new user (Empty)" $ do
      let result = addNewUser logH logLevel responseEmpty listUsers (startRepeat conf) 
      result `shouldBe` return (Map.empty ) 
  describe "Test function getOffset : " $ do
    it "test - get offset (Text)" $ do
      let result = getOffset logH logLevel responseText
      result `shouldBe` return "124"
    it "test - get offset (Photo)" $ do
      let result = getOffset logH logLevel responsePhoto
      result `shouldBe` return "457"
    it "test - get offset (Empty)" $ do
      let result = getOffset logH logLevel responseEmpty
      result `shouldBe` return " "

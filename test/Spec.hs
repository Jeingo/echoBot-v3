{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Data.Functor.Identity (Identity)
import qualified Data.Map as Map

import App.Handle.Request as Request
import App.Config
import Type.Response

main :: IO ()
main = hspec test

handle :: Request.Handle Identity
handle = Request.Handle 
  { sendHelpText = \helpText chatId token -> return () ,
    sendKeyboard = \repeatText keyB chatId token -> return () ,
    sendMessages = \resp token numOfRepeat -> return () ,
    sendStikers = \resp token numOfRepeat -> return () }

listUsers :: Map.Map Int Int
listUsers = Map.empty

token :: Token
token = getToken conf 

conf :: ConfData
conf = ConfData "helpText" "repeatText" 3 "buttonJson" (Token "12345token")

test :: Spec
test = do
  describe "Test : Choose road after request (help , repeat , message ..)" $ do
    it "if get /help :" $ do
           let result = Request.sendEcho handle (RT $ RespText 123 124 "/help") conf listUsers token
           result `shouldBe` return () 

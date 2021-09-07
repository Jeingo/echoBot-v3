{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import qualified Data.Map as Map

import App.Config
import Type.Response

main :: IO ()
main = hspec test

listUsers :: Map.Map Int Int
listUsers = Map.empty

token :: Token
token = getToken conf 

conf :: ConfData
conf = ConfData "helpText" "repeatText" 3 "buttonJson" (Token "12345token")

test :: Spec
test = do
  describe "Tests : " $ do
    it "test 1 :" $ do
           let result = token
           result `shouldBe` token 

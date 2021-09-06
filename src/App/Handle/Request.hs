{-# LANGUAGE OverloadedStrings #-}

module App.Handle.Request where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe (fromJust)

import Type.Response
import App.Config

data Handle m = Handle 
  { sendHelpText :: HelpText -> JustId -> Token -> m () ,
    sendKeyboard :: RepeatText -> KeyBoardJson -> JustId -> Token -> m () ,
    sendMessages :: RespText -> Token -> Int -> m () ,
    sendStikers :: RespStiker -> Token -> Int -> m () }

sendEcho :: Monad m => Handle m -> ResponseAll -> ConfData -> ListUsers -> Token -> m ListUsers
sendEcho handle (RE resp) conf listUsers (Token token ) = do
  return listUsers 

sendEcho handle (RT resp) conf listUsers token = do
  let newUsersList = addNewUser listUsers (getJustId resp) (startRepeat conf) 
  let numOfRepeat = fromJust $ Map.lookup (getJustId resp) newUsersList
  case (getMessage resp) of
    "/help" -> sendHelpText handle (helpText conf) (getJustId resp) token
    "/repeat" -> sendKeyboard handle (repeatText conf) (button conf) (getJustId resp) token
    _ -> sendMessages handle resp token numOfRepeat
  return newUsersList 

sendEcho handle (RB resp) conf listUsers token = do
  let numOfRepeat = read $ T.unpack (getNumRepeat resp) :: Int
  let newUsersList = Map.insert (getJustId resp) numOfRepeat listUsers
  return newUsersList

sendEcho handle (RS resp) conf listUsers token = do
  let newUsersList = addNewUser listUsers (getJustId resp) (startRepeat conf) 
  let numOfRepeat = fromJust $ Map.lookup (getJustId resp) newUsersList
  sendStikers handle resp token numOfRepeat
  return newUsersList

addNewUser :: ListUsers -> JustId -> Int -> ListUsers 
addNewUser listUsers newUser numOfRepeat = if Map.member newUser listUsers
                                        then listUsers
                                        else Map.insert newUser numOfRepeat listUsers


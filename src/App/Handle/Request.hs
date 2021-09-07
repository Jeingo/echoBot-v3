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

sendEcho :: Monad m => Handle m -> ResponseAll -> ConfData -> ListUsers -> Token -> m ()

sendEcho handle (RE resp) conf listUsers (Token token ) = return () 

sendEcho handle (RB resp) conf listUsers token = return () 

sendEcho handle (RT resp) conf listUsers token = do
  let numOfRepeat = fromJust $ Map.lookup (getJustId resp) listUsers 
  case (getMessage resp) of
    "/help" -> sendHelpText handle (helpText conf) (getJustId resp) token
    "/repeat" -> sendKeyboard handle (repeatText conf) (button conf) (getJustId resp) token
    _ -> sendMessages handle resp token numOfRepeat
  return () 

sendEcho handle (RS resp) conf listUsers token = do
  let numOfRepeat = fromJust $ Map.lookup (getJustId resp) listUsers 
  sendStikers handle resp token numOfRepeat
  return () 


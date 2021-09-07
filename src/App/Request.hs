{-# LANGUAGE OverloadedStrings #-}

module App.Request where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B8
import qualified Network.HTTP.Simple as N
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Aeson
import Data.Maybe
import Control.Monad

import Type.Response
import App.Config

-- get Response

urlApiTelegram :: String
urlApiTelegram = "https://api.telegram.org/bot"

getUpdates :: Token -> IO B.ByteString 
getUpdates (Token token) = do
  let req = urlApiTelegram ++ token ++ "/getUpdates" ++ "?timeout=29"
  resp <- N.httpBS $ N.parseRequest_ req 
  return $ N.getResponseBody resp 

-- parse Response

parseResponseToMyType :: B.ByteString -> ResponseAll 
parseResponseToMyType respTmp
  | respE /= Nothing = RE (fromJust respE)
  | respT /= Nothing = RT (fromJust respT) 
  | respB /= Nothing = RB (fromJust respB) 
  | respS /= Nothing = RS (fromJust respS)
  where respT = decodeStrict respTmp :: Maybe RespText
        respB = decodeStrict respTmp :: Maybe RespButton
        respS = decodeStrict respTmp :: Maybe RespStiker
        respE = decodeStrict respTmp :: Maybe RespEmpty

-- send Request

sendEcho :: ResponseAll -> ConfData -> ListUsers -> Token -> IO () 

sendEcho (RE resp) conf listUsers token = return ()

sendEcho (RB resp) conf listUsers token = return ()

sendEcho (RT resp) conf listUsers token = do
  let numOfRepeat = fromJust $ Map.lookup (getJustId resp) listUsers 
  case (getMessage resp) of
    "/help" -> sendHelpText (helpText conf) (getJustId resp) token
    "/repeat" -> sendKeyboard (repeatText conf) (button conf) (getJustId resp) token
    _ -> sendMessages resp token numOfRepeat
  return () 

sendEcho (RS resp) conf listUsers token = do
  let numOfRepeat = fromJust $ Map.lookup (getJustId resp) listUsers 
  sendStikers resp token numOfRepeat
  return () 

-- get offset and next step

getOffset :: ResponseAll -> Offset 
getOffset (RT a) = show $ ( getUpdateId a ) + 1 
getOffset (RB a) = show $ ( getUpdateId a ) + 1 
getOffset (RS a) = show $ ( getUpdateId a ) + 1 
getOffset (RE a) = " "

nextStepRequest :: Offset -> Token -> IO ()
nextStepRequest " " _ = return ()
nextStepRequest offset (Token token ) = do
  let req = urlApiTelegram ++ token ++  "/getUpdates" ++ "?offset=" ++ offset
  N.httpNoBody $ N.parseRequest_ req
  return ()

-- helper for send function 

sendHelpText :: HelpText -> JustId -> Token -> IO ()
sendHelpText helpT chatId (Token token) = do
  let req = urlApiTelegram ++ token ++ "/sendMessage" ++ "?chat_id=" ++ (show chatId) ++ "&text=" ++ helpT 
  N.httpNoBody $ N.parseRequest_ $ req
  return ()

sendKeyboard :: RepeatText -> KeyBoardJson -> JustId -> Token -> IO () 
sendKeyboard repeatText keyButton chId (Token token) = do
  let repText = B8.fromString repeatText
  let chatId = B8.fromString $ show chId 
  let keyB = B8.fromString keyButton
  request' <- N.parseRequest $ "POST " ++ urlApiTelegram ++ token ++ "/sendMessage"
  let req = N.setRequestQueryString [("chat_id", Just $ chatId),
                                     ("text", Just $ repText), 
                                     ("reply_markup", Just keyB)] $ request'
  N.httpNoBody req
  return () 

sendMessages :: RespText -> Token -> Int -> IO ()
sendMessages resp (Token token) numOfRepeat = do
  let chId = show $ getJustId resp
  let textResp = T.unpack $ getMessage resp
  let req = urlApiTelegram ++ token ++ "/sendMessage" ++ "?chat_id=" ++ chId ++ "&text=" ++  textResp 
  replicateM_ numOfRepeat $ N.httpNoBody $ N.parseRequest_ req
  return ()

sendStikers :: RespStiker -> Token -> Int -> IO ()
sendStikers resp (Token token) numOfRepeat = do
  let chId = show $ getJustId resp
  let stikerId = T.unpack $ getStikerId resp
  let req = urlApiTelegram ++ token ++ "/sendAnimation" ++ "?chat_id=" ++ chId ++ "&animation=" ++  stikerId 
  replicateM_ numOfRepeat $ N.httpNoBody $ N.parseRequest_ req
  return ()

-- add new user

addNewUser :: ResponseAll -> ListUsers -> Int -> ListUsers 
addNewUser (RT resp) listUsers numOfRepeat = if Map.member (getJustId resp) listUsers
                                        then listUsers
                                        else Map.insert (getJustId resp) numOfRepeat listUsers

addNewUser (RS resp) listUsers numOfRepeat = if Map.member (getJustId resp) listUsers
                                        then listUsers
                                        else Map.insert (getJustId resp) numOfRepeat listUsers

addNewUser (RB resp) listUsers numOfRepeat = Map.insert (getJustId resp) num listUsers 
  where num = read $ T.unpack (getNumRepeat resp) :: Int 

addNewUser (RE resp) listUsers numOfRepeat = listUsers 


























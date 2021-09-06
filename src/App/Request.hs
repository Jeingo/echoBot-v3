{-# LANGUAGE OverloadedStrings #-}

module App.Request where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B8
import qualified Network.HTTP.Simple as N
import qualified Data.Text as T
import Data.Aeson
import Data.Maybe
import Control.Monad

import qualified App.Handle.Request as Request 
import Type.Response
import App.Config


urlApiTelegram :: String
urlApiTelegram = "https://api.telegram.org/bot"

getUpdates :: Token -> IO B.ByteString 
getUpdates (Token token) = do
  let req = urlApiTelegram ++ token ++ "/getUpdates" ++ "?timeout=29"
  resp <- N.httpBS $ N.parseRequest_ req 
  return $ N.getResponseBody resp 

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

sendEcho :: ResponseAll -> ConfData -> ListUsers -> Token -> IO ListUsers 
sendEcho = undefined 

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

-- For handle IO

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
  let req = urlApiTelegram ++ token ++ "/sendMessage" ++ "?chat_id=" ++ chId ++ "&animation=" ++  stikerId 
  replicateM_ numOfRepeat $ N.httpNoBody $ N.parseRequest_ req
  return ()







































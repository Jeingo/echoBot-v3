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
import App.Logger
import qualified App.Handle.Request as Handle 

-- logger Handle

logHInfo ::Handle.LogHandle IO
logHInfo = Handle.LogHandle { Handle.logger = \level typeLog mess -> logger level INFO mess }

logHWarn ::Handle.LogHandle IO
logHWarn = Handle.LogHandle { Handle.logger = \level typeLog mess -> logger level WARN mess }

-- get Response

urlApiTelegram :: String
urlApiTelegram = "https://api.telegram.org/bot"

getUpdates :: LogLevel -> Token -> IO B.ByteString 
getUpdates logLevel (Token token) = do
  let req = urlApiTelegram ++ token ++ "/getUpdates" ++ "?timeout=29"
  loggerInfo logLevel "make url api -----"
  resp <- N.httpBS $ N.parseRequest_ req 
  loggerInfo logLevel "get update"
  return $ N.getResponseBody resp 

-- parse Response

parseResponseToMyType :: B.ByteString -> LogLevel -> IO ResponseAll 
parseResponseToMyType respTmp logLevel
  | respE /= Nothing = do loggerInfo logLevel "This is empty response" ; return $ RE (fromJust respE)
  | respT /= Nothing = do loggerInfo logLevel "This is text response " ; return $ RT (fromJust respT) 
  | respB /= Nothing = do loggerInfo logLevel "This is button response" ; return $ RB (fromJust respB) 
  | respS /= Nothing = do loggerInfo logLevel "This is stiker response" ; return $ RS (fromJust respS)
  | respVd /= Nothing = do loggerInfo logLevel "This is video response" ; return $ RVd (fromJust respVd)
  | respVo /= Nothing = do loggerInfo logLevel "This is voice response" ; return $ RVo (fromJust respVo)
  | respD /= Nothing = do loggerInfo logLevel "This is document response" ; return $ RD (fromJust respD)
  | respP /= Nothing = do loggerInfo logLevel "This is photo response" ; return $ RP (fromJust respP)
  | respA /= Nothing = do loggerInfo logLevel "This is ohter response" ; return $ RA (fromJust respA)
  where respT = decodeStrict respTmp :: Maybe RespText
        respB = decodeStrict respTmp :: Maybe RespButton
        respS = decodeStrict respTmp :: Maybe RespStiker
        respVd = decodeStrict respTmp :: Maybe RespVideo
        respVo = decodeStrict respTmp :: Maybe RespVoice
        respD = decodeStrict respTmp :: Maybe RespDocument
        respP = decodeStrict respTmp :: Maybe RespPhoto
        respE = decodeStrict respTmp :: Maybe RespEmpty
        respA = decodeStrict respTmp :: Maybe RespAnyForOffset

-- send Request

sendEcho :: LogLevel -> ResponseAll -> ConfData -> ListUsers -> Token -> IO () 

sendEcho logLevel (RE resp) conf listUsers token = return ()

sendEcho logLevel (RB resp) conf listUsers token = return ()

sendEcho logLevel (RA resp) conf listUsers token = return ()

sendEcho logLevel (RT resp) conf listUsers token = do
  let numOfRepeat = fromJust $ Map.lookup (getJustId resp) listUsers 
  case (getMessage resp) of
    "/help" -> do loggerInfo logLevel "send /help text" ; sendHelpText (helpText conf) (getJustId resp) token
    "/repeat" -> do loggerInfo logLevel "send /repeat text" ; sendKeyboard (repeatText conf) (button conf) (getJustId resp) token
    _ -> do loggerInfo logLevel "send message text" ; sendMessages resp token numOfRepeat
  return () 

sendEcho logLevel (RS resp) conf listUsers token = do
  loggerInfo logLevel "send stiker"
  let numOfRepeat = fromJust $ Map.lookup (getJustId resp) listUsers 
  sendStikers resp token numOfRepeat
  return () 

sendEcho logLevel (RVd resp) conf listUsers token = do
  loggerInfo logLevel "send video"
  let numOfRepeat = fromJust $ Map.lookup (getJustId resp) listUsers 
  sendVideo resp token numOfRepeat
  return () 

sendEcho logLevel (RVo resp) conf listUsers token = do
  loggerInfo logLevel "send voice"
  let numOfRepeat = fromJust $ Map.lookup (getJustId resp) listUsers 
  sendVoice resp token numOfRepeat
  return () 

sendEcho logLevel (RD resp) conf listUsers token = do
  loggerInfo logLevel "send document"
  let numOfRepeat = fromJust $ Map.lookup (getJustId resp) listUsers 
  sendDocument resp token numOfRepeat
  return () 

sendEcho logLevel (RP resp) conf listUsers token = do
  loggerInfo logLevel "send photo"
  let numOfRepeat = fromJust $ Map.lookup (getJustId resp) listUsers 
  sendPhoto resp token numOfRepeat
  return () 

-- get offset and next step

getOffset :: Handle.LogHandle IO -> LogLevel ->  ResponseAll -> IO Offset
getOffset logH logLevel resp = Handle.getOffset logH logLevel resp

nextStepRequest :: LogLevel -> Offset -> Token -> IO ()
nextStepRequest _ " " _ = return ()
nextStepRequest logLevel offset (Token token ) = do
  let req = urlApiTelegram ++ token ++  "/getUpdates" ++ "?offset=" ++ offset
  N.httpNoBody $ N.parseRequest_ req
  loggerInfo logLevel "next step"
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
  let fileId = T.unpack $ getFileId resp
  let req = urlApiTelegram ++ token ++ "/sendAnimation" ++ "?chat_id=" ++ chId ++ "&animation=" ++  fileId 
  replicateM_ numOfRepeat $ N.httpNoBody $ N.parseRequest_ req
  return ()

sendVideo :: RespVideo -> Token -> Int -> IO ()
sendVideo resp (Token token) numOfRepeat = do
  let chId = show $ getJustId resp
  let fileId = T.unpack $ getFileId resp
  let req = urlApiTelegram ++ token ++ "/sendVideo" ++ "?chat_id=" ++ chId ++ "&video=" ++  fileId 
  replicateM_ numOfRepeat $ N.httpNoBody $ N.parseRequest_ req
  return ()
  
sendVoice :: RespVoice -> Token -> Int -> IO ()
sendVoice resp (Token token) numOfRepeat = do
  let chId = show $ getJustId resp
  let fileId = T.unpack $ getFileId resp
  let req = urlApiTelegram ++ token ++ "/sendVoice" ++ "?chat_id=" ++ chId ++ "&voice=" ++  fileId 
  replicateM_ numOfRepeat $ N.httpNoBody $ N.parseRequest_ req
  return ()

sendDocument :: RespDocument -> Token -> Int -> IO ()
sendDocument resp (Token token) numOfRepeat = do
  let chId = show $ getJustId resp
  let fileId = T.unpack $ getFileId resp
  let req = urlApiTelegram ++ token ++ "/sendDocument" ++ "?chat_id=" ++ chId ++ "&document=" ++  fileId 
  replicateM_ numOfRepeat $ N.httpNoBody $ N.parseRequest_ req
  return ()

sendPhoto :: RespPhoto -> Token -> Int -> IO ()
sendPhoto resp (Token token) numOfRepeat = do
  let chId = show $ getJustId resp
  let fileId = T.unpack $ getFileId resp
  let req = urlApiTelegram ++ token ++ "/sendPhoto" ++ "?chat_id=" ++ chId ++ "&photo=" ++  fileId 
  replicateM_ numOfRepeat $ N.httpNoBody $ N.parseRequest_ req
  return ()

-- add new user

addNewUser :: Handle.LogHandle IO -> LogLevel -> ResponseAll -> ListUsers -> Int -> IO ListUsers
addNewUser logH logLevel resp listUsers numOfRepeat = Handle.addNewUser logH logLevel resp listUsers numOfRepeat







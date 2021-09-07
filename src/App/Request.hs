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
  | respVd /= Nothing = RVd (fromJust respVd)
  | respVo /= Nothing = RVo (fromJust respVo)
  | respD /= Nothing = RD (fromJust respD)
  | respP /= Nothing = RP (fromJust respP)
  | respA /= Nothing = RA (fromJust respA)
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

sendEcho :: ResponseAll -> ConfData -> ListUsers -> Token -> IO () 

sendEcho (RE resp) conf listUsers token = return ()

sendEcho (RB resp) conf listUsers token = return ()

sendEcho (RA resp) conf listUsers token = return ()

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

sendEcho (RVd resp) conf listUsers token = do
  let numOfRepeat = fromJust $ Map.lookup (getJustId resp) listUsers 
  sendVideo resp token numOfRepeat
  return () 

sendEcho (RVo resp) conf listUsers token = do
  let numOfRepeat = fromJust $ Map.lookup (getJustId resp) listUsers 
  sendVoice resp token numOfRepeat
  return () 

sendEcho (RD resp) conf listUsers token = do
  let numOfRepeat = fromJust $ Map.lookup (getJustId resp) listUsers 
  sendDocument resp token numOfRepeat
  return () 

sendEcho (RP resp) conf listUsers token = do
  let numOfRepeat = fromJust $ Map.lookup (getJustId resp) listUsers 
  sendPhoto resp token numOfRepeat
  return () 

-- get offset and next step

getOffset :: ResponseAll -> Offset 
getOffset (RT a) = show $ ( getUpdateId a ) + 1 
getOffset (RB a) = show $ ( getUpdateId a ) + 1 
getOffset (RS a) = show $ ( getUpdateId a ) + 1 
getOffset (RA a) = show $ ( getUpdateId a ) + 1
getOffset (RVd a) = show $ ( getUpdateId a ) + 1
getOffset (RVo a) = show $ ( getUpdateId a ) + 1
getOffset (RD a) = show $ ( getUpdateId a ) + 1
getOffset (RP a) = show $ ( getUpdateId a ) + 1
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

addNewUser :: ResponseAll -> ListUsers -> Int -> ListUsers 
addNewUser (RT resp) listUsers numOfRepeat = if Map.member (getJustId resp) listUsers
                                        then listUsers
                                        else Map.insert (getJustId resp) numOfRepeat listUsers

addNewUser (RS resp) listUsers numOfRepeat = if Map.member (getJustId resp) listUsers
                                        then listUsers
                                        else Map.insert (getJustId resp) numOfRepeat listUsers

addNewUser (RVd resp) listUsers numOfRepeat = if Map.member (getJustId resp) listUsers
                                        then listUsers
                                        else Map.insert (getJustId resp) numOfRepeat listUsers

addNewUser (RVo resp) listUsers numOfRepeat = if Map.member (getJustId resp) listUsers
                                        then listUsers
                                        else Map.insert (getJustId resp) numOfRepeat listUsers

addNewUser (RD resp) listUsers numOfRepeat = if Map.member (getJustId resp) listUsers
                                        then listUsers
                                        else Map.insert (getJustId resp) numOfRepeat listUsers

addNewUser (RP resp) listUsers numOfRepeat = if Map.member (getJustId resp) listUsers
                                        then listUsers
                                        else Map.insert (getJustId resp) numOfRepeat listUsers

addNewUser (RB resp) listUsers numOfRepeat = Map.insert (getJustId resp) num listUsers 
  where num = read $ T.unpack (getNumRepeat resp) :: Int 

addNewUser (RE resp) listUsers numOfRepeat = listUsers 

addNewUser (RA resp) listUsers numOfRepeat = listUsers


























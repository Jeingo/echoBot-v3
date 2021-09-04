{-# LANGUAGE OverloadedStrings #-}

module App.Request where

import qualified Data.ByteString as B
import qualified Network.HTTP.Simple as N

import qualified App.Handle.Request as Handle
import Type.Response
import App.Config


urlApiTelegram :: String
urlApiTelegram = "https://api.telegram.org/bot"

getUpdates :: Token -> IO B.ByteString 
getUpdates (Token token) = do
  let req = urlApiTelegram ++ token ++ "/getUpdates" ++ "?timeout=29"
  resp <- N.httpBS $ N.parseRequest_ req 
  return $ N.getResponseBody resp 

sendEcho = undefined

getOffset = undefined

nextStepRequest = undefined

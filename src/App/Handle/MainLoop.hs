{-# LANGUAGE OverloadedStrings #-}

module App.Handle.MainLoop where

import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe

import App.Handle.Request
import App.Config


type ListUsers = Map.Map Int Int
type ResponseB = B.ByteString

data Handle m = Handle 
  { mainLoop :: ConfData -> ListUsers -> m () }
                       
{-- 
whatTheType :: B.ByteString -> String 
whatTheType resp
  | (makeResponse resp :: Maybe ReqBool) /= Nothing = "empty"
  | (makeResponse resp :: Maybe ReqText) /= Nothing = "text"
  | (makeResponse resp :: Maybe ReqButton) /= Nothing = "button"
  | (makeResponse resp :: Maybe ReqStiker) /= Nothing = "stiker"
--}

chooseRoad :: Monad m => Handle m -> ConfData -> ListUsers -> ResponseB -> String -> m ()
chooseRoad handle conf listUsers resp typeResp
  | typeResp == "empty" = emptyFunc handle conf listUsers
  | typeResp == "text" = textFunc
  | typeResp == "button" = buttonFunc
  | typeResp == "stiker" = stikerFunc
  | otherwise = otherFunc

emptyFunc :: Handle m -> ConfData -> ListUsers -> m () 
emptyFunc handle conf listUsers = ( mainLoop handle ) conf listUsers

textFunc :: Handle m -> ConfData -> ListUsers -> ResponseB -> m ()
textFunc handle conf listUsers respB = do
  let response = fromJust (makeResponse respB :: Maybe ReqText )
  let chatId = justIdT response
  let beginRepeat = startRepeat conf
  let users = addNewUser listUsers chatId beginRepeat 
  let message = messageT response
  return ()

buttonFunc = undefined
stikerFunc = undefined
otherFunc = undefined
   

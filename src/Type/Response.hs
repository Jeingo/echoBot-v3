{-# LANGUAGE OverloadedStrings #-}

module Type.Response where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.Aeson
import Control.Monad
import Data.Maybe


type ListUsers = Map.Map Int Int

type UpdateId = Int
type JustId = Int
type Offset = String 
type Message = T.Text
type NumRepeat = T.Text
type StikerId = T.Text
type HelpText = String
type RepeatText = String
type KeyBoardJson = String

data ResponseAll = RT RespText | RB RespButton | RS RespStiker | RE RespEmpty deriving (Show, Eq)
data RespText = RespText UpdateId JustId Message deriving (Show, Eq)
data RespButton = RespButton UpdateId JustId NumRepeat deriving (Show, Eq)
data RespStiker = RespStiker UpdateId JustId StikerId deriving (Show, Eq)
data RespEmpty = RespEmpty () deriving (Show, Eq)

class Response a where
  getUpdateId :: a -> UpdateId
  getJustId :: a -> JustId
    
instance Response RespText where
  getUpdateId (RespText upId _ _) = upId 
  getJustId (RespText _ jId _) = jId
 
instance Response RespButton where
  getUpdateId (RespButton upId _ _) = upId 
  getJustId (RespButton _ jId _) = jId

instance Response RespStiker where
  getUpdateId (RespStiker upId _ _) = upId 
  getJustId (RespStiker _ jId _) = jId

getMessage :: RespText -> Message
getMessage (RespText _ _ mess) = mess

getNumRepeat :: RespButton -> NumRepeat
getNumRepeat (RespButton _ _ numR) = numR

getStikerId :: RespStiker -> StikerId
getStikerId (RespStiker _ _ stkId) = stkId

--Json

instance FromJSON RespText where
  parseJSON (Object req) = do 
    result <- req .: "result"
    let arr = V.head result
    upId <- arr .: "update_id"
    mes <- arr .: "message"
    mesg <- mes .: "text" 
    from <- mes .: "from"
    jId <- from .: "id"
    return $ RespText upId jId mesg 

  parseJSON _ = mzero

instance FromJSON RespStiker where
  parseJSON (Object req) = do 
    result <- req .: "result"
    let arr = V.head result
    upId <- arr .: "update_id"
    mes <- arr .: "message"
    stick <- mes .: "sticker" 
    from <- mes .: "from"
    jId <- from .: "id"
    fId <- stick .: "file_id"
    return $ RespStiker upId jId fId 

  parseJSON _ = mzero

instance FromJSON RespButton where
  parseJSON (Object req) = do
    result <- req .: "result"
    let arr = V.head result
    upId <- arr .: "update_id"
    callback <- arr .: "callback_query"
    count <- callback .: "data"
    from <- callback .: "from"
    jId <- from .: "id"
    return $ RespButton upId jId count

  parseJSON _ = mzero


instance FromJSON RespEmpty where
  parseJSON (Object req) = do
    result <- req .: "result"
    return $ RespEmpty result

  parseJSON _ = mzero




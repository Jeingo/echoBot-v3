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
type FileId = T.Text
type HelpText = String
type RepeatText = String
type KeyBoardJson = String


data ResponseAll = RT RespText | RB RespButton | RS RespStiker | RE RespEmpty | RA RespAnyForOffset | RVd RespVideo | RVo RespVoice | RD RespDocument | RP RespPhoto deriving (Show, Eq)
data RespText = RespText UpdateId JustId Message deriving (Show, Eq)
data RespButton = RespButton UpdateId JustId NumRepeat deriving (Show, Eq)
data RespStiker = RespStiker UpdateId JustId FileId deriving (Show, Eq)
data RespVideo = RespVideo UpdateId JustId FileId deriving (Show, Eq)
data RespVoice = RespVoice UpdateId JustId FileId deriving (Show, Eq)
data RespDocument = RespDocument UpdateId JustId FileId deriving (Show, Eq)
data RespPhoto = RespPhoto UpdateId JustId FileId deriving (Show, Eq)
data RespEmpty = RespEmpty () deriving (Show, Eq)
data RespAnyForOffset = RespAny UpdateId deriving (Show, Eq)

class Response a where
  getUpdateId :: a -> UpdateId
  getJustId :: a -> JustId
  getFileId :: a -> FileId
    
instance Response RespText where
  getUpdateId (RespText upId _ _) = upId 
  getJustId (RespText _ jId _) = jId
  getFileId a = " "
 
instance Response RespButton where
  getUpdateId (RespButton upId _ _) = upId 
  getJustId (RespButton _ jId _) = jId
  getFileId a = " "

instance Response RespStiker where
  getUpdateId (RespStiker upId _ _) = upId 
  getJustId (RespStiker _ jId _) = jId
  getFileId (RespStiker _ _ stikId) = stikId

instance Response RespAnyForOffset where
  getUpdateId (RespAny upId) = upId 
  getJustId (RespAny _) = 0
  getFileId a = " "

instance Response RespVideo where
  getUpdateId (RespVideo upId _ _) = upId 
  getJustId (RespVideo _ jId _) = jId
  getFileId (RespVideo _ _ vidId) = vidId

instance Response RespVoice where
  getUpdateId (RespVoice upId _ _) = upId 
  getJustId (RespVoice _ jId _) = jId
  getFileId (RespVoice _ _ voId) = voId
    
instance Response RespDocument where
  getUpdateId (RespDocument upId _ _) = upId 
  getJustId (RespDocument _ jId _) = jId
  getFileId (RespDocument _ _ docId) = docId
  
instance Response RespPhoto where
  getUpdateId (RespPhoto upId _ _) = upId 
  getJustId (RespPhoto _ jId _) = jId
  getFileId (RespPhoto _ _ phId) = phId

getMessage :: RespText -> Message
getMessage (RespText _ _ mess) = mess

getNumRepeat :: RespButton -> NumRepeat
getNumRepeat (RespButton _ _ numR) = numR

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

instance FromJSON RespAnyForOffset where
  parseJSON (Object req) = do
    result <- req .: "result"
    let arr = V.head result
    upId <- arr .: "update_id"
    return $ RespAny upId

  parseJSON _ = mzero

instance FromJSON RespVideo where
  parseJSON (Object req) = do 
    result <- req .: "result"
    let arr = V.head result
    upId <- arr .: "update_id"
    mes <- arr .: "message"
    video <- mes .: "video" 
    from <- mes .: "from"
    jId <- from .: "id"
    fId <- video .: "file_id"
    return $ RespVideo upId jId fId 

  parseJSON _ = mzero

instance FromJSON RespVoice where
  parseJSON (Object req) = do 
    result <- req .: "result"
    let arr = V.head result
    upId <- arr .: "update_id"
    mes <- arr .: "message"
    voice <- mes .: "voice" 
    from <- mes .: "from"
    jId <- from .: "id"
    fId <- voice .: "file_id"
    return $ RespVoice upId jId fId 

  parseJSON _ = mzero

instance FromJSON RespDocument where
  parseJSON (Object req) = do 
    result <- req .: "result"
    let arr = V.head result
    upId <- arr .: "update_id"
    mes <- arr .: "message"
    doc <- mes .: "document" 
    from <- mes .: "from"
    jId <- from .: "id"
    fId <- doc .: "file_id"
    return $ RespDocument upId jId fId 

  parseJSON _ = mzero

instance FromJSON RespPhoto where
  parseJSON (Object req) = do 
    result <- req .: "result"
    let arr = V.head result
    upId <- arr .: "update_id"
    mes <- arr .: "message"
    photo <- mes .: "photo" 
    from <- mes .: "from"
    jId <- from .: "id"
    let fstPh = V.head photo 
    fId <- fstPh .: "file_id"
    return $ RespPhoto upId jId fId 

  parseJSON _ = mzero

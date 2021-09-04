module Type.Response where

import qualified Data.Text as T
import qualified Data.Map as Map

type ListUsers = Map.Map Int Int

type UpdateId = Int
type JustId = Int
type Message = T.Text
type NumRepeat = T.Text
type FileId = T.Text

data RespText = RespText UpdateId JustId Message deriving (Show, Eq)
data RespButton = RespButton UpdateId JustId NumRepeat deriving (Show, Eq)
data RespStiker = RespStiker UpdateId JustId FileId deriving (Show, Eq)
data RespEmpty = RespEmpty () deriving (Show, Eq)

class Response a where
  getUpdateId :: a -> UpdateId
  getJustId :: a -> JustId

instance Response RespText where
  getUpdateId (RespText upId _ _) = upId 
  getJustId (RespText _ jId _) = jId


module Type.Response where

import qualified Data.Text as T
import qualified Data.Map as Map

type ListUsers = Map.Map Int Int

type UpdateId = Int
type JustId = Int

data RespText = RespTex UpdateId JustId T.Text deriving (Show, Eq)
data RespButton = RespButton UpdateId JustId T.Text deriving (Show, Eq)
data RespStiker = RespStiker UpdateId JustId T.Text deriving (Show, Eq)
data RespEmpty = RespEmpty () deriving (Show, Eq)

class Response a where
  getUpdateId :: a -> UpdateId
  getJustId :: a -> JustId

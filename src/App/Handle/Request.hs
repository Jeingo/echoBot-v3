module App.Handle.Request where

import qualified Data.ByteString as B

class Response a where
  makeResponse :: B.ByteString -> a

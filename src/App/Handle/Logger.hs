module App.Handle.Logger where

import Type.Response

data LogHandle m = LogHandle 
  { log :: LogLevel -> String -> m () }

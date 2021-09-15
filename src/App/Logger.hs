module App.Logger where

import Type.Response
import System.IO
import Data.Time

getTimeMain :: IO String
getTimeMain = do
  now <- getCurrentTime
  let format = formatTime defaultTimeLocale "%Y-%m-%d & %H:%M " now 
  return format

logger :: LogLevel -> LogLevel -> String -> IO ()

logger INFO INFO message = do
  time <- getTimeMain 
  let logs = time ++ "[INFO]:" ++ message ++ "\n"
  appendFile "src/log/logs" logs

logger WARN INFO _ = return ()

logger WARN WARN message = do
  time <- getTimeMain 
  let logs = time ++ "[WARN]:" ++ message ++ "\n"
  appendFile "src/log/logs" logs

logger DEBUG INFO message = do
  time <- getTimeMain 
  let logs = time ++ "[INFO]:" ++ message ++ "\n"
  appendFile "src/log/logs" logs

logger INFO WARN _ = return ()

logger DEBUG WARN message = do
  time <- getTimeMain 
  let logs = time ++ "[WARN]:" ++ message ++ "\n"
  appendFile "src/log/logs" logs

loggerInfo :: LogLevel -> String -> IO ()
loggerInfo logLevel message = logger logLevel INFO message 

loggerWarn :: LogLevel -> String -> IO ()
loggerWarn logLevel message = logger logLevel WARN message

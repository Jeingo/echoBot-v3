module Main where

import qualified Data.Map as Map (empty)

import App.Config
import App.MainLoop

main :: IO ()
main = do
  myConfigTmp <- readConfig
  myConfig <- makeMyConfig myConfigTmp
  let emptyListUsers = Map.empty
  mainLoop myConfig emptyListUsers 
  return ()

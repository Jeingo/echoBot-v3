{-# LANGUAGE OverloadedStrings #-}

module App.MainLoop where

import Type.Response
import App.Config
import App.Request


mainLoop :: ConfData -> ListUsers -> IO ()
mainLoop conf listUsers = do

  let token = getToken conf

  responseTmp <- getUpdates token 
  print responseTmp
  let response = parseResponseToMyType responseTmp

  newListUsers <- sendEcho response conf listUsers token

  let responseOffset = getOffset response 
  nextStepRequest responseOffset token

  mainLoop conf newListUsers


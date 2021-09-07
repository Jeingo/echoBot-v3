{-# LANGUAGE OverloadedStrings #-}

module App.MainLoop where

import Type.Response
import App.Config
import App.Request


mainLoop :: ConfData -> ListUsers -> IO ()
mainLoop conf listUsers = do

  let token = getToken conf

  responseTmp <- getUpdates token 
  let response = parseResponseToMyType responseTmp  

  let newListUsers = addNewUser response listUsers (startRepeat conf)
  sendEcho response conf newListUsers token

  let responseOffset = getOffset response 
  nextStepRequest responseOffset token

  mainLoop conf newListUsers


{-# LANGUAGE OverloadedStrings #-}

module App.MainLoop where

import Type.Response
import App.Config
import App.Request


mainLoop :: ConfData -> ListUsers -> IO ()
mainLoop conf listUsers = do

  let token = getToken conf
  let logLevel = getLogLevel conf

  responseTmp <- getUpdates logH token 
  let response = parseResponseToMyType responseTmp  
  
  newListUsers <- addNewUser logH response listUsers (startRepeat conf)

  sendEcho response conf newListUsers token
 
  responseOffset <- getOffset logH response 
  nextStepRequest responseOffset token

  mainLoop conf newListUsers


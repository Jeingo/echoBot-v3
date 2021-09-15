{-# LANGUAGE OverloadedStrings #-}

module App.MainLoop where

import Type.Response
import App.Config
import App.Request


mainLoop :: ConfData -> ListUsers -> IO ()
mainLoop conf listUsers = do

  let token = getToken conf
  let logLevel = getLogLevel conf

  responseTmp <- getUpdates logLevel token 
  response <- parseResponseToMyType responseTmp logLevel
  
  newListUsers <- addNewUser logHInfo logLevel response listUsers (startRepeat conf)

  sendEcho logLevel response conf newListUsers token
 
  responseOffset <- getOffset logHInfo logLevel response 
  nextStepRequest logLevel responseOffset token

  mainLoop conf newListUsers


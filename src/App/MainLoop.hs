{-# LANGUAGE OverloadedStrings #-}

module App.MainLoop where

import Data.Maybe

import Type.Response
import App.Config
import App.Request


mainLoop :: ConfData -> ListUsers -> IO ()
mainLoop conf listUsers = do

  let token = getToken conf

  responseTmp <- getUpdates token 
  let response = fromJust $ parseResponseToMyType responseTmp 

  newListUsers <- sendEcho response conf listUsers token

  let responseOffset = getOffset response 
  nextStepRequest responseOffset token

  mainLoop conf newListUsers

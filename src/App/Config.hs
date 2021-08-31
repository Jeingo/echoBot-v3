{-# LANGUAGE OverloadedStrings #-}

module App.Config where

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.Text as T

newtype Token = Token String deriving (Show, Eq)

data ConfData = ConfData { helpText :: String
                         , repeatText :: String
                         , startRepeat :: Int 
                         , button :: String
                         , getToken :: Token
                         } deriving (Show, Eq)


readConfig :: IO CT.Config 
readConfig = C.load [C.Required "src/config/confBot.cfg"]

readToken :: IO Token
readToken = do
  con <- readFile "src/config/token"
  return $ Token $ filter (/= '\n') con

makeMyConfig :: CT.Config -> IO ConfData
makeMyConfig conf = do
  hT <- C.require conf (T.pack "main.helpText") :: IO String 
  rT <- C.require conf (T.pack "main.repeatText") :: IO String
  sR <- C.require conf (T.pack "main.startRepeat") :: IO Int 
  btTmp <- C.require conf (T.pack "main.countButton") :: IO Int 
  let bt = makeButton btTmp
  tok <- readToken
  return $ ConfData hT rT sR bt tok


-- button generator

buttonTemplateStart :: String -> String
buttonTemplateStart temp = "{\"inline_keyboard\":[[" ++ temp ++ "]]}"

buttonTemplate :: Int -> String
buttonTemplate num = "{\"text\":\"" ++ numT ++ "\",\"callback_data\":\"" ++ numT ++ "\"}" 
  where numT = show num

buttonGenerate :: Int -> String
buttonGenerate 0 = []
buttonGenerate 1 = buttonTemplate 1
buttonGenerate num = buttonGenerate (num - 1) ++ " , " ++ (buttonTemplate num)

makeButton :: Int -> String
makeButton num = buttonTemplateStart $ buttonGenerate num


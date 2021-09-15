module App.Handle.Request where

import qualified Data.Map as Map
import qualified Data.Text as T
import Type.Response

data LogHandle m = LogHandle
  { logger :: LogLevel -> LogLevel -> String -> m () }

addNewUser :: Monad m => LogHandle m -> LogLevel -> ResponseAll -> ListUsers -> Int -> m ListUsers 
addNewUser logH logLevel (RT resp) listUsers numOfRepeat = if Map.member (getJustId resp) listUsers
                                        then do logger logH logLevel INFO "it's the same user" ; return listUsers
                                        else do logger logH logLevel INFO "add new user" ; return $ Map.insert (getJustId resp) numOfRepeat listUsers

addNewUser logH logLevel (RS resp) listUsers numOfRepeat = if Map.member (getJustId resp) listUsers
                                        then do logger logH logLevel INFO "it's the same user" ; return listUsers
                                        else do logger logH logLevel INFO "add new user" ; return $ Map.insert (getJustId resp) numOfRepeat listUsers

addNewUser logH logLevel (RVd resp) listUsers numOfRepeat = if Map.member (getJustId resp) listUsers
                                        then do logger logH logLevel INFO "it's the same user" ; return listUsers
                                        else do logger logH logLevel INFO "add new user" ; return $ Map.insert (getJustId resp) numOfRepeat listUsers

addNewUser logH logLevel (RVo resp) listUsers numOfRepeat = if Map.member (getJustId resp) listUsers
                                        then do logger logH logLevel INFO "it's the same user" ; return listUsers
                                        else do logger logH logLevel INFO "add new user" ; return $ Map.insert (getJustId resp) numOfRepeat listUsers

addNewUser logH logLevel (RD resp) listUsers numOfRepeat = if Map.member (getJustId resp) listUsers
                                        then do logger logH logLevel INFO "it's the same user" ; return listUsers
                                        else do logger logH logLevel INFO "add new user" ; return $ Map.insert (getJustId resp) numOfRepeat listUsers

addNewUser logH logLevel (RP resp) listUsers numOfRepeat = if Map.member (getJustId resp) listUsers
                                        then do logger logH logLevel INFO "it's the same user" ; return listUsers
                                        else do logger logH logLevel INFO "add new user" ; return $ Map.insert (getJustId resp) numOfRepeat listUsers

addNewUser logH logLevel (RB resp) listUsers numOfRepeat = do logger logH logLevel INFO "change number of repeat for user " ; return $ Map.insert (getJustId resp) num listUsers 
  where num = read $ T.unpack (getNumRepeat resp) :: Int 

addNewUser logH logLevel (RE resp) listUsers numOfRepeat = return listUsers 

addNewUser logH logLevel (RA resp) listUsers numOfRepeat = return listUsers


getOffset :: Monad m => LogHandle m -> LogLevel -> ResponseAll -> m Offset 
getOffset logH logLevel (RT a) = do logger logH logLevel INFO "get offset" ; return $ show $ ( getUpdateId a ) + 1 
getOffset logH logLevel (RB a) = do logger logH logLevel INFO "get offset" ; return $ show $ ( getUpdateId a ) + 1
getOffset logH logLevel (RS a) = do logger logH logLevel INFO "get offset" ; return $ show $ ( getUpdateId a ) + 1
getOffset logH logLevel (RA a) = do logger logH logLevel INFO "get offset" ; return $ show $ ( getUpdateId a ) + 1
getOffset logH logLevel (RVd a) = do logger logH logLevel INFO "get offset" ; return $ show $ ( getUpdateId a ) + 1
getOffset logH logLevel (RVo a) = do logger logH logLevel INFO "get offset" ; return $ show $ ( getUpdateId a ) + 1
getOffset logH logLevel (RD a) = do logger logH logLevel INFO "get offset" ; return $ show $ ( getUpdateId a ) + 1
getOffset logH logLevel (RP a) = do logger logH logLevel INFO "get offset" ; return $ show $ ( getUpdateId a ) + 1
getOffset logH logLevel (RE a) = return " "



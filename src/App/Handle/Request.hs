module App.Handle.Request where

import qualified Data.Map as Map
import qualified Data.Text as T
import Type.Response

data LogHandle m = LogHandle
  { logger :: LogLevel -> String -> m () }

addNewUser :: Monad m => LogHandle m -> ResponseAll -> ListUsers -> Int -> m ListUsers 
addNewUser logH (RT resp) listUsers numOfRepeat = if Map.member (getJustId resp) listUsers
                                        then return listUsers
                                        else return $ Map.insert (getJustId resp) numOfRepeat listUsers

addNewUser logH (RS resp) listUsers numOfRepeat = if Map.member (getJustId resp) listUsers
                                        then return listUsers
                                        else return $ Map.insert (getJustId resp) numOfRepeat listUsers

addNewUser logH (RVd resp) listUsers numOfRepeat = if Map.member (getJustId resp) listUsers
                                        then return listUsers
                                        else return $ Map.insert (getJustId resp) numOfRepeat listUsers

addNewUser logH (RVo resp) listUsers numOfRepeat = if Map.member (getJustId resp) listUsers
                                        then return listUsers
                                        else return $ Map.insert (getJustId resp) numOfRepeat listUsers

addNewUser logH (RD resp) listUsers numOfRepeat = if Map.member (getJustId resp) listUsers
                                        then return listUsers
                                        else return $ Map.insert (getJustId resp) numOfRepeat listUsers

addNewUser logH (RP resp) listUsers numOfRepeat = if Map.member (getJustId resp) listUsers
                                        then return listUsers
                                        else return $ Map.insert (getJustId resp) numOfRepeat listUsers

addNewUser logH (RB resp) listUsers numOfRepeat = return $ Map.insert (getJustId resp) num listUsers 
  where num = read $ T.unpack (getNumRepeat resp) :: Int 

addNewUser logH (RE resp) listUsers numOfRepeat = return listUsers 

addNewUser logH (RA resp) listUsers numOfRepeat = return listUsers


getOffset :: Monad m => LogHandle m -> ResponseAll -> m Offset 
getOffset logH (RT a) = return $ show $ ( getUpdateId a ) + 1 
getOffset logH (RB a) = return $ show $ ( getUpdateId a ) + 1
getOffset logH (RS a) = return $ show $ ( getUpdateId a ) + 1
getOffset logH (RA a) = return $ show $ ( getUpdateId a ) + 1
getOffset logH (RVd a) = return $ show $ ( getUpdateId a ) + 1
getOffset logH (RVo a) = return $ show $ ( getUpdateId a ) + 1
getOffset logH (RD a) = return $ show $ ( getUpdateId a ) + 1
getOffset logH (RP a) = return $ show $ ( getUpdateId a ) + 1
getOffset logH (RE a) = return " "



module LogTools (writeToLog
                ,finalLog_ue
                ,finalLog_mme
                ,closeLog_mme)
       where

import Data.Time
import System.IO
import RrcMessages
import S1Messages
import UeContextStates
import Network.Socket
import qualified Data.Map.Lazy as Map
import UeContextStates
import Control.Monad.IO.Class
import Control.Concurrent.STM

type MmeMap = Map.Map Int UeContext_mme

writeToLog :: Handle -> String -> IO ()
writeToLog handle message = do
  time <- getCurrentTime
  hPutStrLn handle (show time ++ " : " ++ message)

finalLog_ue :: Handle -> UeContext_ue -> (RrcMessage, Socket) -> IO()
finalLog_ue handle state (message, _) = do
  writeToLog handle ("UE Context at the end : "++ show state)

finalLog_mme :: Handle -> IO (TVar MmeMap) -> (S1APMessage, Socket) -> IO()
finalLog_mme handle mmeState (message, mmeSocket) = do
  liftedState <- liftIO mmeState
  map <- readTVarIO liftedState
  let
    key = eNB_UE_S1AP_Id message
    lastContext = Map.findWithDefault defaultUeContext_mme key map
  writeToLog handle ("Context at the end : "++ show lastContext)

closeLog_mme :: Handle ->(S1APMessage, Socket)-> IO()
closeLog_mme handle (_,mmeSocket)= do
  liftIO $ close mmeSocket
  hClose handle

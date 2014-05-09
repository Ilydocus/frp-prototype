module LogTools (writeToLog
                ,finalLog_ue
                ,finalLog_enb
                ,closeLog_enb 
                ,finalLog_mme
                ,closeLog_mme)
       where

import Data.Time
import System.IO
import RrcMessages
import S1Messages
import UeContextStates
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString.Lazy
import qualified Data.Map.Lazy as Map
import UeContextStates
import Control.Monad.IO.Class
import Control.Concurrent.STM

type MmeMap = Map.Map Int UeContext_mme
type EnbMap = Map.Map Int UeContext_enb

writeToLog :: Handle -> String -> IO ()
writeToLog handle message = do
  time <- getCurrentTime
  hPutStrLn handle (show time ++ " : " ++ message)

finalLog_ue :: Handle -> UeContext_ue -> (RrcMessage, Socket) -> IO()
finalLog_ue handle state (message, _) = do
  writeToLog handle ("UE Context at the end: "++ show state)

finalLog_enb :: Handle -> IO (TVar EnbMap) -> RrcMessage -> IO()
finalLog_enb handle state message = do
  liftedState <- liftIO state
  map <- readTVarIO liftedState
  let
    key = ueCRnti message
    lastContext = Map.findWithDefault defaultUeContext_enb key map
  writeToLog handle ("UE Context at the end: "++ show lastContext)

closeLog_enb :: Handle -> (RrcMessage, Socket, Socket) ->IO()
closeLog_enb handle (_,ueSocket,mmeSocket) = do
   _ <- send mmeSocket $ encode (EndOfProgramMme)
   liftIO $ close ueSocket
   hClose handle

finalLog_mme :: Handle -> IO (TVar MmeMap) -> (S1ApMessage, Socket) -> IO()
finalLog_mme handle mmeState (message, mmeSocket) = do
  liftedState <- liftIO mmeState
  map <- readTVarIO liftedState
  let
    key = enb_Ue_S1Ap_Id message
    lastContext = Map.findWithDefault defaultUeContext_mme key map
  writeToLog handle ("UE Context at the end: "++ show lastContext)

closeLog_mme :: Handle ->(S1ApMessage, Socket)-> IO()
closeLog_mme handle (_,mmeSocket)= do
  liftIO $ close mmeSocket
  hClose handle

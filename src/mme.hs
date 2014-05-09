{-# LANGUAGE ScopedTypeVariables #-} --allows "forall t. NetworkDescription t"
module Main (main) where

import Control.Monad (when)
import System.IO
import Network hiding (accept)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString.Lazy
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Map.Lazy as Map

import Reactive.Banana as R
import Reactive.Banana.Frameworks as R
import EventSources

import S1Messages
import UeContextStates
import Identifiers
import LogTools

type MmeMap = Map.Map Int UeContext_mme


main :: IO ()
main = do
  sources <- newAddHandler
  mmeMap <- newTVarIO Map.empty
  logh <- openFile "log_mme.txt" WriteMode
  network <- compile $ setupNetwork sources (return mmeMap) logh
  actuate network
  eventLoop sources 

eventLoop :: EventSource (S1ApMessage, Socket)-> IO ()
eventLoop message = withSocketsDo $ do
  listenSocket <- listenOn $ PortNumber 43001
  forever $ do
    (connectionSocket, _) <- accept listenSocket
    forkIO $ do
      forever $ do
        messDec <- decode <$> recv connectionSocket 1024
        (fire message (messDec,connectionSocket))
     
{-----------------------------------------------------
     Program Logic
------------------------------------------------------}

setupNetwork :: forall t. Frameworks t =>
   EventSource (S1ApMessage,Socket) -> IO (TVar MmeMap)-> Handle -> Moment t ()
setupNetwork message state logh = do
  eMessage <- fromAddHandler (addHandler message)
  let
    --Counter of messages received
    bNbMessages :: Behavior t Int
    eNbMessages :: Event t Int
    (eNbMessages, bNbMessages) =
      mapAccum 0 . fmap (\f x -> (f x, f x)) $ (addMessage <$ eMessage)
      where
        addMessage = (+ 1)

    --Behavior holding the state
    bUeContexts :: Behavior t (IO (TVar MmeMap))
    bUeContexts = pure state
    
    initUeContext :: IO(TVar MmeMap) -> (S1ApMessage, Socket) -> IO ()
    initUeContext state (message, socket) = do
      liftedState <- liftIO $ state
      atomically $ modifyTVar liftedState (\mmeMap -> Map.insert (enb_Ue_S1Ap_Id message) (initialUeContext_mme (mme_Ue_S1Ap_Id message) (S1Messages.securityKey message)) mmeMap )

    incomingMessageType :: (S1ApMessage, Socket) -> S1MessageType
    incomingMessageType event = case event of
      (S1ApInitialUeMessage a b c,_) -> S1ApIUeM
      (S1ApInitialContextSetupResponse a b,_)-> S1ApICSResponse
      (EndOfProgramMme,_)-> EndProgMme

    eInitialUeMessage = filterE (\t -> (incomingMessageType t) == S1ApIUeM) eMessage
    eInitialContextSetupResponse = filterE (\t -> (incomingMessageType t) == S1ApICSResponse) eMessage
    eEndOfProgram = filterE (\t -> (incomingMessageType t) == EndProgMme) eMessage   
    
    eResponseS1ApIUeM =
      createS1ApICSRequest <$> eInitialUeMessage
      where
        createS1ApICSRequest (message,mmeSocket) =
          (S1ApInitialContextSetupRequest ((enb_Ue_S1Ap_Id message)-4) (enb_Ue_S1Ap_Id message) (((enb_Ue_S1Ap_Id message)`div`17)*18) (genRandId 7 ((enb_Ue_S1Ap_Id message)-4)),mmeSocket)

  --Output
  reactimate $ sendResponse <$> eResponseS1ApIUeM
  reactimate $ (initUeContext <$> bUeContexts) <@> eResponseS1ApIUeM
  reactimate $ writeToLog logh . showMessage <$> eMessage
  reactimate $ writeToLog logh . showNbMessages <$> eNbMessages
  reactimate $ (finalLog_mme logh <$> bUeContexts)  <@> eInitialContextSetupResponse
  reactimate $ closeLog_mme logh <$> eEndOfProgram
  
showNbMessages :: Int -> String    
showNbMessages nbMessages = "Total number of messages received in the MME: " ++ show nbMessages

showMessage :: (S1ApMessage,Socket) -> String
showMessage (message, _) = "Message received:  " ++ show message

sendResponse :: (S1ApMessage,Socket) -> IO ()
sendResponse (message,sock) = do
                             _ <- send sock $ encode message
                             return ()


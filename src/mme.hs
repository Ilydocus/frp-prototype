{-# LANGUAGE ScopedTypeVariables #-} --allows "forall t. NetworkDescription t"
module Main (main) where

import Control.Monad (when)
import System.IO

import Reactive.Banana as R
import Reactive.Banana.Frameworks as R

--Socket related
import Network hiding (accept)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString.Lazy

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

import qualified Data.Map.Lazy as Map

--import RrcMessages
import S1Messages
import UeContextStates
import Identifiers

import LogTools

type MMEMap = Map.Map Int UeContext_mme


main :: IO ()
main = do
  sources <- newAddHandler
  mmeMap <- newTVarIO Map.empty
  logh <- openFile "log_mme.txt" WriteMode
  network <- compile $ setupNetwork sources (return mmeMap) logh
  actuate network
  eventLoop sources -- pourquoi a la fin?

--Read commands and fire corresponding events (Approximation nb1)
eventLoop :: EventSource (S1APMessage, Socket)-> IO ()
eventLoop message = loop
   where
     loop = withSocketsDo $ do
       listenSocket <- listenOn $ PortNumber 43001
       forever $ do
         (connectionSocket, _) <- accept listenSocket
         forkIO $ do
          forever $ do
           messDec <- decode <$> recv connectionSocket 1024
           (fire message (messDec,connectionSocket))
     
{-----------------------------------------------------
     Event Sources
------------------------------------------------------}
type EventSource a = (AddHandler a, a -> IO())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO()
fire = snd

{-----------------------------------------------------
     Program Logic
------------------------------------------------------}

-- Set up the event network
setupNetwork :: forall t. Frameworks t =>
   EventSource (S1APMessage,Socket) -> IO (TVar MMEMap)-> Handle -> Moment t ()
setupNetwork message database logh = do

  --Obtain events corresponding to the two commands
  eMessage{-:: Event t (RrcMessage,Socket)-} <- fromAddHandler (addHandler message)

  let
    --How to capture the state? Compare to the slot machine
    --For now, number of messages received is the state
    -- both the eMessage 1 and the emessage2 events add one to the state
    bNbMessages :: Behavior t Int
    eNbMessages :: Event t Int
    (eNbMessages, bNbMessages) = mapAccum 0 . fmap (\f x -> (f x, f x)) $
                     (addMessage <$ eMessage)
                         
    --Functions to change the state
    addMessage = (+ 1)

    bUeContexts :: Behavior t (IO (TVar MMEMap))
    bUeContexts = stepper database (initUeContexts database <$> eSendRepInitialMessage)

    initUeContexts :: IO(TVar MMEMap) -> (S1APMessage, Socket) -> IO (TVar MMEMap)
    initUeContexts database (message, socket) = do
      tempDatabase <- liftIO $ database
      atomically $ modifyTVar tempDatabase (\mmeMap -> Map.insert (eNB_UE_S1AP_Id message) (defaultUeContext (mme_UE_S1AP_Id message)(S1Messages.securityKey message)) mmeMap )
      --return the entry that was created ?
      database

    --Inner functions to fire a specific event depending on the one that was received
    --one needeed per incoming message

    --Functions firing a specific id depending on the incoming message type
    incomingMessageType :: (S1APMessage, Socket) -> S1MessageType
    incomingMessageType event = case event of
      (S1APInitialUEMessage a b c,_) -> InitUEMess
      (S1APInitialContextSetupRequest a b c d,_)-> InitContextReq
      (S1APInitialContextSetupResponse a b,_)-> InitContextRes
      (UplinkNASTransport a, _)-> UplinkTrans

    --Fire specific intern event for each message  
    eInitialUEMessage ::Event t (S1APMessage,Socket)
    eInitialUEMessage = filterE (\t -> (incomingMessageType t) ==InitUEMess) eMessage

    eUplinkNASTransport ::Event t (S1APMessage,Socket)
    eUplinkNASTransport = filterE (\t -> (incomingMessageType t) ==UplinkTrans) eMessage

    eInitialContextSetupResponse ::Event t (S1APMessage,Socket)
    eInitialContextSetupResponse = filterE (\t -> (incomingMessageType t) ==InitContextRes) eMessage

    --Create messages to send back to the eNodeB
    eSendRepInitialMessage :: Event t (S1APMessage, Socket)
    eSendRepInitialMessage = createMessageInitialSetup <$>eInitialUEMessage
  
    createMessageInitialSetup (message,mmeSocket) =      
       (S1APInitialContextSetupRequest ((eNB_UE_S1AP_Id message)-4) (eNB_UE_S1AP_Id message) (((eNB_UE_S1AP_Id message)`div`17)*18) (genRandId 7 ((eNB_UE_S1AP_Id message)-4)),mmeSocket)

{-    eMessageRrcCR ::Event t (RrcMessage,Socket)
    eMessageRrcCR = filterE rrcCR eMessage

    rrcCR:: (RrcMessage, Socket)-> Bool
    rrcCR x = case x of
      (RRCConnectionRequest m b c, _)-> True
      _ -> False

    eMessageRrcCC ::Event t (RrcMessage,Socket)
    eMessageRrcCC = filterE rrcCC eMessage

    rrcCC:: (RrcMessage, Socket)-> Bool
    rrcCC x = case x of
      (RRCConnectionSetupComplete m, _)-> True
      _ -> False

  --case eMessage of
  -- (RAPreamble m, Socket s) -> eMessageRAPreamble m s

    -}

    --Output: Is printed in the order of definition?
  reactimate $ putStrLn . showNbMessages <$> eNbMessages
  reactimate $ putStrLn . showMessageNumber <$> eMessage
 -- reactimate $ sendResponse (RAResponse RA_RNTI 12 21) <$> eMessageRAPreamble
  --reactimate $ sendResponse (RRCConnectionSetup C_RNTI 45 54) <$> eMessageRrcCR
  reactimate $ putStrLn "InitialUEMessage received " <$ eInitialUEMessage
  reactimate $ sendResponse <$> eSendRepInitialMessage
  --reactimate $ sendResponse (RRCConnectii True) <$> eMessageRrcCC
  --reactimate $ putStrLn  "ConnectionSetup is complete " <$ eMessageRrcCC
    --add an output event?
  reactimate $ writeToLog logh . showMessageNumber <$> eMessage
  --reactimate $ hClose logh <$  eSendRepReconfComplete
  reactimate $ (finalLog logh <$> bUeContexts)  <@> eInitialContextSetupResponse
    
showNbMessages nbMessages = "Nb of messages received: " ++ show nbMessages
showMessageNumber (number, socket) = "MME: Message from UE " ++ show number ++ " has arrived"
sendResponse (message,sock) = do
                             _ <- send sock $ encode message
                             return ()

defaultUeContext mmeId securityKey= UeContext_mme{
  mmeUES1APid = mmeId,
  securityKey_mme = securityKey
  }

defaultEmptyContext = UeContext_mme{
  mmeUES1APid = -1,
  securityKey_mme = -1
  }

finalLog :: Handle -> IO(TVar MMEMap) -> (S1APMessage, Socket) -> IO()
finalLog handle behaviorContent (message, _) = do
  tempDatabase <- liftIO behaviorContent
  map <- readTVarIO tempDatabase
  let
    key = eNB_UE_S1AP_Id message
    --lastContext :: UeContext_eNB
    lastContext = Map.findWithDefault defaultEmptyContext key map
  writeToLog handle ("Context at the end : "++ show lastContext)
  --hClose handle

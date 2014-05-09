{-# LANGUAGE ScopedTypeVariables #-} 
module Main (main) where

import Control.Monad (when)
import System.IO
import Network hiding (accept)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString.Lazy
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Exception
import qualified Data.Map.Lazy as Map
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Binary
import Data.Binary.Put
import Codec.Crypto.AES

import Reactive.Banana as R
import Reactive.Banana.Frameworks as R
import EventSources

import RrcMessages
import S1Messages as S1
import Identifiers
import UeContextStates as Context
import LogTools

type EnbMap = Map.Map Int UeContext_eNB

main :: IO ()
main = do
  sources <- makeSources 
  enbMap <- newTVarIO Map.empty 
  logh <- openFile "log_enb.txt" WriteMode
  network <- compile $ setupNetwork sources (return enbMap) logh  
  actuate network
  eventLoop sources 

makeSources = (,) <$> newAddHandler <*> newAddHandler 

eventLoop :: (EventSource (RrcMessage, Socket,Socket), EventSource (S1APMessage,Socket,Socket))-> IO ()
eventLoop (messageUe, messageMme) = withSocketsDo $ do
  listenSocket <- listenOn $ PortNumber 43000
  forever $ do
    (connectionSocket, _) <- accept listenSocket
    forkIO $ do
      bracket (connectedMMESocket "127.0.0.1" "43001")
              close (powerMME connectionSocket)
      where
        powerMME :: Socket  -> Socket -> IO ()
        powerMME ueSock mmeSock = do
          forkIO $ do
            forever $ do
              messDecUe <- decode <$> recv ueSock 1024
              (fire messageUe (messDecUe,ueSock,mmeSock))
          forever $  do
              messDecMme <- decode <$> recv mmeSock 1024
              (fire messageMme (messDecMme,ueSock,mmeSock))
          return ()      

{-----------------------------------------------------
     Program Logic
------------------------------------------------------}

setupNetwork :: forall t. Frameworks t =>
   (EventSource (RrcMessage,Socket,Socket), EventSource(S1APMessage, Socket,Socket))-> IO(TVar EnbMap) -> Handle -> Moment t ()
setupNetwork (messageUe, messageMme) state logh = do
  eMessageUe <- fromAddHandler (addHandler messageUe)
  eMessageMme <- fromAddHandler (addHandler messageMme)
  let
    --Counter of messages received
    bNbMessages :: Behavior t Int
    eNbMessages :: Event t Int
    (eNbMessages, bNbMessages) =
      mapAccum 0 . fmap (\f x -> (f x, f x)) $ ((addMessage <$ eMessageUe)`union`(addMessage <$ eMessageMme))
      where
        addMessage = (+ 1)

    --Behavior holding the state
    bUeContexts :: Behavior t (IO (TVar EnbMap))
    bUeContexts = pure state
    
    initUeContext :: IO (TVar EnbMap) -> (RrcMessage, Socket) -> IO ()
    initUeContext state (message, ueSocket) = do
      liftedState <- liftIO $ state
      atomically $ modifyTVar liftedState (\eNBmap -> Map.insert (ueIdCRnti message) (initialUeContext_enb (ueIdCRnti message)) eNBmap )

    addImsi :: IO(TVar EnbMap) -> (RrcMessage,Socket,Socket) -> IO ()
    addImsi state (message,_,_)= do
      liftedState <- liftIO state
      atomically $ modifyTVar liftedState (\ueMap -> Map.adjust (addImsi_enb (ueIdentity message)) (ueIdRntiValue message) ueMap)

    addSrb :: IO (TVar EnbMap) -> (RrcMessage, Socket) -> IO ()
    addSrb state (RRCConnectionSetup _ key srb,_)= do
      liftedState <- liftIO state
      atomically $ modifyTVar liftedState (\ueMap -> Map.adjust (addSrb_enb srb) key ueMap)

    --Incoming messages  
    incomingMessageTypeUe :: (RrcMessage,Socket,Socket) -> RrcMessageType
    incomingMessageTypeUe event = case event of
      (RAPreamble _ _,_,_)-> RaP
      (RRCConnectionRequest _ _ _, _,_)-> RrcCRequest
      (RRCConnectionSetupComplete _ _,_,_)-> RrcCSC
      (SecurityModeComplete _ _,_,_)-> SecurityMComplete
      (UECapabilityInformation _ _,_,_)-> UeCI
      (RRCConnectionReconfigurationComplete _ _,_,_)-> RrcCRC
      (EndOfProgram,_,_)->EndProgEnb
    incomingMessageTypeMme :: (S1APMessage,Socket,Socket) -> S1MessageType
    incomingMessageTypeMme event = case event of
      (S1APInitialContextSetupRequest _ _ _ _,_,_) -> S1ApICSRequest

    eRaPreamble = filterE (\t -> (incomingMessageTypeUe t) == RaP) eMessageUe
    eRrcConnectionRequest = filterE (\t -> (incomingMessageTypeUe t) == RrcCRequest) eMessageUe
    eRrcConnectionSetupComplete = filterE (\t -> (incomingMessageTypeUe t) == RrcCSC) eMessageUe
    eSecurityModeComplete = filterE (\t -> (incomingMessageTypeUe t) == SecurityMComplete) eMessageUe
    eUeCapabilityInformation = filterE (\t -> (incomingMessageTypeUe t) == UeCI) eMessageUe
    eRrcConnectionReconfigurationComplete = filterE (\t -> (incomingMessageTypeUe t) == RrcCRC) eMessageUe
    eS1ApInitialContextSetupRequest = filterE (\t -> (incomingMessageTypeMme t) == S1ApICSRequest) eMessageMme
    eEndOfProgram = filterE (\t -> (incomingMessageTypeUe t) == EndProgEnb) eMessageUe

    --Responses
    eResponseRaP =
      createRaResponse <$> eRaPreamble
      where
        createRaResponse (message,ueSock,_) = do
          (RAResponse RA_RNTI (ueIdRntiValue message) (ueIdCRnti message),ueSock)

    eResponseRrcCRequest =
      createRrcCS <$> eRrcConnectionRequest
      where
        createRrcCS (message, ueSocket, _) =
          if reject
          then (RRCConnectionReject crnti ((crnti `mod` 15)+ 1),ueSocket)
          else (RRCConnectionSetup C_RNTI crnti srbId,ueSocket)
          where
            crnti = ueIdRntiValue message
            srbId = genRandId 8 (crnti *4)
            reject = (crnti `mod` 30)== 0
        
    --IO Responses        
    eResponseRrcCSC =
      (createS1ApIUeM <$> bUeContexts) <@> eRrcConnectionSetupComplete
      where
        createS1ApIUeM state (message,_,mmeSocket) = do
          liftedState <- liftIO state
          atomically $ modifyTVar liftedState (\ueMap -> Map.adjust (changeStateAddEnbId_enb enbUeId) (ueCRnti message) ueMap)
          map <- readTVarIO liftedState
          return(S1APInitialUEMessage enbUeId EPSAttach (imsi (Map.findWithDefault defaultUeContext_enb (ueCRnti message) map)),mmeSocket)
          where
            enbUeId= 17*(ueCRnti message)
               
    eResponseS1ApICSRequest =
      (createSecurityMCommand <$> bUeContexts) <@> eS1ApInitialContextSetupRequest
      where
        createSecurityMCommand :: IO(TVar EnbMap)-> (S1APMessage,Socket,Socket)-> IO(RrcMessage,Socket)
        createSecurityMCommand state (message,ueSocket,_) = do
          liftedState <- liftIO state
          atomically $ modifyTVar liftedState (\ueMap -> Map.adjust (addSecurityKeyEpsId_enb (S1.securityKey message) (S1.epsBearerId message)) ((eNB_UE_S1AP_Id message)`div`17) ueMap)
          return (SecurityModeCommand ((eNB_UE_S1AP_Id message)`div`17) (encryptString (S1.securityKey message) "ciphered"),ueSocket)
    
    eResponseSecurityMComplete =
      (createUeCE <$> bUeContexts) <@> eSecurityModeComplete
      where
        createUeCE :: IO(TVar EnbMap)->(RrcMessage,Socket,Socket) -> IO(RrcMessage,Socket)
        createUeCE state (message, ueSocket, _) =
          if (securityModeSuccess message)
          then do
            return (UECapabilityEnquiry crnti [E_UTRA,UTRA,GERAN_CS,GERAN_PS,CDMA2000],ueSocket)
          else do
            _ <- changeStateToIdle crnti state
            return (RRCConnectionReject crnti ((crnti `mod` 15)+ 1),ueSocket)
            where
              crnti = ueCRnti message
        
    changeStateToIdle key state = do
     liftedState <- liftIO state
     atomically $ modifyTVar liftedState (\ueMap -> Map.adjust changeStateToIdle_enb key ueMap)
         
    eResponseUeCI =
      (createRrcCReconfiguration <$> bUeContexts) <@> eUeCapabilityInformation
      where
        createRrcCReconfiguration :: IO(TVar EnbMap)-> (RrcMessage,Socket,Socket)-> IO(RrcMessage,Socket)
        createRrcCReconfiguration state (message,ueSocket,_) = do
          liftedState <- liftIO state
          atomically $ modifyTVar liftedState (\ueMap -> Map.adjust (addRat_enb (ueCapabilityRatList message)) (ueCRnti message) ueMap)
          map <- readTVarIO liftedState
          return (RRCConnectionReconfiguration (ueCRnti message) (Context.epsBearerId (Map.findWithDefault defaultUeContext_enb (ueCRnti message) map)) ,ueSocket)       

    eResponseRrcCRC =
      (createResponseRrcCRC <$> bUeContexts) <@> eRrcConnectionReconfigurationComplete
      where
        createResponseRrcCRC :: IO(TVar EnbMap) -> (RrcMessage,Socket,Socket) -> IO(LastMessage)
        createResponseRrcCRC state (message, ueSocket, mmeSocket) =
          if (epsRadioBearerActivated message)
          then do
            enbId <- liftIO $ getEnbId state crnti
            finalLog_enb logh state message
            return (ReconfigurationCompleted (S1APInitialContextSetupResponse enbId (genRandId 5 (enbId +45)),mmeSocket,(RRCConnectionAccept crnti),ueSocket))
          else do
            _ <- changeStateToIdle crnti state
            finalLog_enb logh state message
            return (ReconfigurationFailed (RRCConnectionReject crnti ((crnti `mod` 15)+ 1),ueSocket))
          where
            crnti = ueCRnti message
            getEnbId state key= do
              liftedState <- liftIO state
              map <- readTVarIO liftedState
              return (eNBUES1APid (Map.findWithDefault defaultUeContext_enb key map))

    eResponseMessage = eResponseRaP `union` eResponseRrcCRequest
    eResponseMessageIO = eResponseS1ApICSRequest `union` eResponseSecurityMComplete `union` eResponseUeCI 
       
  --Output
  reactimate $ (sendResponse logh <$> bUeContexts) <@> eResponseMessage
  reactimate $ sendResponseIO <$> eResponseMessageIO
  reactimate $ sendResponseIOmme <$> eResponseRrcCSC
  reactimate $ sendResponseLast <$>  eResponseRrcCRC
  reactimate $ (initUeContext <$> bUeContexts) <@> eResponseRaP
  reactimate $ (addImsi <$> bUeContexts) <@> eRrcConnectionRequest
  reactimate $ (addSrb <$> bUeContexts) <@> eResponseRrcCRequest 
  reactimate $ writeToLog logh . showMessageUe <$> eMessageUe
  reactimate $ writeToLog logh . showMessageMme <$> eMessageMme
  reactimate $ writeToLog logh . showNbMessages <$> eNbMessages
  reactimate $ closeLog_enb logh <$> eEndOfProgram

showNbMessages :: Int -> String    
showNbMessages nbMessages = "Total number of messages received in the eNodeB: " ++ show nbMessages

showMessageUe :: (RrcMessage,Socket,Socket) -> String
showMessageUe (message,_,_) = "Message received from UE: " ++ show message

showMessageMme :: (S1APMessage,Socket,Socket) -> String
showMessageMme (message,_ ,_) = "Message received from MME: " ++ show message

sendResponse ::  Handle ->IO(TVar EnbMap)-> (RrcMessage,Socket)-> IO ()
sendResponse handle state (message, ueSocket) =
  case message of
    (RRCConnectionReject key _) -> do
      liftedState <- liftIO state
      map <- readTVarIO liftedState
      let lastContext =  Map.findWithDefault defaultUeContext_enb key map
      writeToLog handle ("UE Context at the end: "++ (show lastContext))
      _ <- send ueSocket $ encode message 
      return ()
    _ -> do
      _ <- send ueSocket $ encode message 
      return ()

sendResponseIO ::IO (RrcMessage,Socket) -> IO()
sendResponseIO x = do
  (message, socket)<- liftIO x
  _ <- send socket $ encode message
  return ()

sendResponseIOmme ::IO (S1APMessage,Socket) -> IO()
sendResponseIOmme x = do
  (message, socket)<- liftIO x
  _ <- send socket $ encode message
  return ()
                            
sendResponseLast ::  IO LastMessage-> IO ()
sendResponseLast x = do
  messageComp<- liftIO x
  case messageComp of
    ReconfigurationFailed (message,ueSock) -> do
                                         _ <- send ueSock $ encode message
                                         liftIO $ close ueSock
                                         return ()
    ReconfigurationCompleted (messageMme,mmeSock, messageUe, ueSock) -> do
          _ <- send mmeSock $ encode messageMme
          _ <- send ueSock $ encode messageUe
          liftIO $ close ueSock
          return ()

encryptString :: Int -> String -> BL.ByteString
encryptString key message =
  crypt CTR key' iv Encrypt (encode message)
  where
    key'= B.concat (BL.toChunks(BL.concat[(encode key), (encode key)]))
    iv = B.concat (BL.toChunks(runPut(do
                                         putWord64be 0
                                         putWord64be 0
                                     )))
  
connectedMMESocket :: HostName -> ServiceName -> IO Socket
connectedMMESocket host port = do
  sock <- socket AF_INET Stream defaultProtocol
  addrInfo <- getAddrInfo Nothing (Just host) (Just port)
  let primary = head addrInfo
  connect sock $ addrAddress primary
  return sock



  
  

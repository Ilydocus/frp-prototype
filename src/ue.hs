{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Exception
import Control.Applicative
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString.Lazy
import Control.Concurrent.Async
import Data.Time
import System.IO
import Control.Monad
import System.Random
import Codec.Crypto.AES
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Word
import Data.Binary.Put

import Reactive.Banana as R
import Reactive.Banana.Frameworks as R
import EventSources

import RrcMessages
import Identifiers
import UeContextStates
import LogTools

main :: IO ()
main =
  withSocketsDo $ do    
    let nbOfUes = 100
    logh <- openFile "log_ue.txt" WriteMode
    
    startTime <- getCurrentTime
    ues <- mapM (async . powerOn logh) [1..nbOfUes] 
    mapM_ wait ues
    endTime <- getCurrentTime

    endOfProgram   
    let diff = endTime `diffUTCTime` startTime
    writeToLog logh $ "Time for the " ++ (show nbOfUes) ++ " UEs procedures: " ++ (show diff)
    hClose logh

powerOn :: Handle -> Int -> IO ()
powerOn logh ueId =
  bracket (connectedSocket "127.0.0.1" "43000")
          close (powerOn' logh)
  where
    powerOn' :: Handle -> Socket -> IO ()
    powerOn' logh sock = do
      sources <- newAddHandler
      network <- compile $ setupNetwork sources ueId logh
      actuate network
      startProcedure <- getCurrentTime
      eventLoop sources sock ueId
      endProcedure <- getCurrentTime
      let diff = endProcedure `diffUTCTime` startProcedure    
      writeToLog logh $  "Terminated  UE " ++ (show ueId) ++ " execution time(UeSide): " ++ (show diff) ++ " with begin time " ++ (show startProcedure)
      return ()

connectedSocket :: HostName -> ServiceName -> IO Socket
connectedSocket host port = do
  sock <- socket AF_INET Stream defaultProtocol
  addrInfo <- getAddrInfo Nothing (Just host) (Just port)
  let primary = head addrInfo
  connect sock $ addrAddress primary
  return sock

endOfProgram :: IO()
endOfProgram =
  bracket (connectedSocket "127.0.0.1" "43000")
          close endOfProgram'
  where
    endOfProgram' :: Socket -> IO ()
    endOfProgram' sock = do
      _ <- send sock $ encode (EndOfProgramEnb)
      return ()

eventLoop :: EventSource (RrcMessage,Socket)-> Socket-> Int -> IO ()
eventLoop message sock ueId = withSocketsDo $ do
       _ <- send sock $ encode (RaPreamble RA_RNTI ueId)
       loop
       where loop = do
               messDec <- decode <$> recv sock 1024
               fire message (messDec,sock)
               when (notlastMessage messDec) loop
               where notlastMessage mess = case mess of
                       (RrcConnectionAccept _ ) -> False
                       (RrcConnectionReject _ _) -> False
                       _ -> True
         
{-----------------------------------------------------
     Program Logic
------------------------------------------------------}

setupNetwork :: forall t. Frameworks t =>
   EventSource (RrcMessage,Socket) -> Int -> Handle -> Moment t ()
setupNetwork message ueId logh = do
  eMessage<- fromAddHandler (addHandler message)
  let
    bUeState :: Behavior t UeContext_ue
    bUeState = stepper (initialUeState ueId) (apply (addSrb_ue <$> bUeState) eRrcConnectionSetup)

    incomingMessageType :: (RrcMessage, Socket) -> RrcMessageType
    incomingMessageType event = case event of
      (RaResponse _ _ _,_)-> RaR
      (RrcConnectionSetup _ _ _, _)-> RrcCS
      (SecurityModeCommand _ _,_)-> SecurityMCommand
      (UeCapabilityEnquiry _ _,_)-> UeCE
      (RrcConnectionReconfiguration _ _,_)-> RrcCReconfiguration
      (RrcConnectionReject _ _,_)-> RrcCReject
      (RrcConnectionAccept _,_)-> RrcCA
       
    eRaResponse = filterE (\t -> (incomingMessageType t) == RaR) eMessage
    eRrcConnectionSetup = filterE (\t -> (incomingMessageType t) == RrcCS) eMessage
    eSecurityModeCommand = filterE (\t -> (incomingMessageType t) == SecurityMCommand) eMessage
    eUeCapabilityEnquiry = filterE (\t -> (incomingMessageType t) == UeCE) eMessage
    eRrcConnectionReconfiguration = filterE (\t -> (incomingMessageType t) == RrcCReconfiguration) eMessage

    eResponseRaR =
      (createRrcCRequest <$> bUeState) <@> eRaResponse
      where
        createRrcCRequest state (message,socket) =
          (RrcConnectionRequest C_RNTI (ueIdCRnti message) (imsi_ue state),socket)
        
    eResponseRrcCS =
      (createRrcCSC <$> bUeState) <@> eRrcConnectionSetup
      where
        createRrcCSC state (message,socket) =
          (RrcConnectionSetupComplete (ueIdRntiValue message) (mcc(imsi_ue state) ++ mnc(imsi_ue state)),socket)
      
    eResponseSecurityMCommand =
      (createSecurityMComplete <$> bUeState) <@> eSecurityModeCommand
      where
        createSecurityMComplete state (message,socket) =
          if (decode decrypted == "ciphered")
          then (SecurityModeComplete (ueCRnti message) True,socket)
          else (SecurityModeComplete (ueCRnti message) False,socket)
          where
            decrypted = decryptResponse (securityKey_ue state) (message_security message)

    eResponseUeCE =
      createUeCI <$> eUeCapabilityEnquiry
      where
        createUeCI (message,socket) =
          (UeCapabilityInformation  (ueCRnti message) (genRandomRatCapabilities ((ueCRnti message)*6) message) ,socket)
          where
            genRandomRatCapabilities seed message =
              zip ratList boolList
              where
                boolList = take 5 $ randoms (mkStdGen seed):: [Bool]
                ratList = (ueCapabilityRequest message)

    eResponseRrcCReconfiguration =
      createRrcCRC <$> eRrcConnectionReconfiguration
      where
        createRrcCRC (message,socket) =
          if activationComplete
          then (RrcConnectionReconfigurationComplete (ueCRnti message) True,socket)
          else (RrcConnectionReconfigurationComplete (ueCRnti message) False,socket)
          where
            epsId = epsRadioBearerIdentity message
            activationComplete = not (((head epsId)== (last epsId)) && ((head epsId)== '9'))
    
    eResponseMessage = eResponseRaR `union` eResponseRrcCS `union` eResponseSecurityMCommand `union` eResponseUeCE `union` eResponseRrcCReconfiguration
         
  --Output
  reactimate $ sendResponse  <$> eResponseMessage
  reactimate $ writeToLog logh . showMessage <$> eMessage
  reactimate $ (finalLog_ue logh <$> bUeState)  <@> eResponseRrcCReconfiguration 

showMessage :: (RrcMessage,Socket) -> String   
showMessage (message,_) = "Message received: " ++ show message 

sendResponse :: (RrcMessage,Socket) -> IO ()
sendResponse (message,sock) = do
                             _ <- send sock $ encode message
                             return ()
                             
decryptResponse :: Int -> BL.ByteString -> BL.ByteString 
decryptResponse key message = 
  crypt CTR key' iv Decrypt message
  where
    key' = B.concat(BL.toChunks(BL.concat[(encode key),(encode key)]))
    iv = B.concat(BL.toChunks(runPut(do
                                        putWord64be 0
                                        putWord64be 0
                                        )))



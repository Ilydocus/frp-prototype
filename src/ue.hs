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

    logh <- openFile "log_ue.txt" WriteMode
    
    startTime <- getCurrentTime
    ues <- mapM (async . powerOn logh) [1..100] --nb of UEs
    mapM_ wait ues
    endTime <- getCurrentTime

    endOfProgram   
    let diff = endTime `diffUTCTime` startTime
    writeToLog logh $ "Time for the UEs procedures: " ++ (show diff)
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
      eventLoop sources sock ueId
      writeToLog logh $  "Terminated  UE " ++ (show ueId)
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
      _ <- send sock $ encode (EndOfProgram)
      return ()

eventLoop :: EventSource (RrcMessage,Socket)-> Socket-> Int -> IO ()
eventLoop message sock ueId = withSocketsDo $ do
       _ <- send sock $ encode (RAPreamble RA_RNTI ueId)
       loop
       where loop = do
               messDec <- decode <$> recv sock 1024
               fire message (messDec,sock)
               when (notlastMessage messDec) loop
               where notlastMessage mess = case mess of
                       (RRCConnectionAccept _ ) -> False
                       (RRCConnectionReject _ _) -> False
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
    bUeState = stepper (defaultUeState ueId) (apply (addSrb <$> bUeState) eMessageRrcCS)

    addSrb :: UeContext_ue -> (RrcMessage,Socket) -> UeContext_ue
    addSrb oldState (message, _)= UeContext_ue{
  imsi_ue = imsi_ue oldState,
  securityKey_ue = securityKey_ue oldState,
  srbId = RrcMessages.srbIdentity message
  }
       
    --Inner functions depending on the incoming event
    eMessageRAResponse ::Event t (RrcMessage,Socket)
    eMessageRAResponse = filterE raResponse eMessage

    raResponse:: (RrcMessage, Socket)-> Bool
    raResponse x = case x of
      (RAResponse a b c, _)-> True
      _ -> False

    eMessageRrcCS ::Event t (RrcMessage,Socket)
    eMessageRrcCS = filterE rrcCS eMessage

    rrcCS:: (RrcMessage, Socket)-> Bool
    rrcCS x = case x of
      (RRCConnectionSetup a b c, _)-> True
      _ -> False

    eMessageRrcReject ::Event t (RrcMessage,Socket)
    eMessageRrcReject = filterE rrcReject eMessage

    rrcReject:: (RrcMessage, Socket)-> Bool
    rrcReject x = case x of
      (RRCConnectionReject a b, _)-> True
      _ -> False
    
    eMessageSecurityModeCommand ::Event t (RrcMessage,Socket)
    eMessageSecurityModeCommand = filterE secCom eMessage

    secCom:: (RrcMessage, Socket)-> Bool
    secCom x = case x of
      (SecurityModeCommand a b, _)-> True
      _ -> False

    eMessageUeCapabilityEnquiry ::Event t (RrcMessage,Socket)
    eMessageUeCapabilityEnquiry = filterE capEnq eMessage

    capEnq:: (RrcMessage, Socket)-> Bool
    capEnq x = case x of
      (UECapabilityEnquiry a b, _)-> True
      _ -> False

    eMessageRrcConnReconf ::Event t (RrcMessage,Socket)
    eMessageRrcConnReconf = filterE connReconf eMessage

    connReconf:: (RrcMessage, Socket)-> Bool
    connReconf x = case x of
      (RRCConnectionReconfiguration a b, _)-> True
      _ -> False

    eMessageRrcConnAccept ::Event t (RrcMessage,Socket)
    eMessageRrcConnAccept = filterE connAccept eMessage

    connAccept:: (RrcMessage, Socket)-> Bool
    connAccept x = case x of
      (RRCConnectionAccept a, _)-> True
      _ -> False

    --Test
    eMessageRAP ::Event t (RrcMessage,Socket)
    eMessageRAP = filterE raPr eMessage

    raPr:: (RrcMessage, Socket)-> Bool
    raPr x = case x of
      (RAPreamble a b, _)-> True
      _ -> False

    eSendMessRrcCR :: Event t (RrcMessage, Socket) 
    eSendMessRrcCR = apply (createRrcCRMessage <$> bUeState) eMessageRAResponse

    createRrcCRMessage behaviorContent (message,socket) =
      ((RRCConnectionRequest C_RNTI (ueTempCRntiValue message) (imsi_ue behaviorContent)),socket)

    eSendMessRrcCSC :: Event t (RrcMessage, Socket) 
    eSendMessRrcCSC = apply (createRrcCSCMessage <$> bUeState) eMessageRrcCS

    createRrcCSCMessage behaviorContent (message,socket) =
      ((RRCConnectionSetupComplete (ueIdRntiValue message) (mcc(imsi_ue behaviorContent)++mnc(imsi_ue behaviorContent))),socket)
    eSendMessSecComplete :: Event t (RrcMessage, Socket) 
    eSendMessSecComplete = apply (createSecCompleteMessage <$> bUeState) eMessageSecurityModeCommand

    createSecCompleteMessage behaviorContent (message,socket) =
      --Decrypt the message
      
      if (decode decrypted == "ciphered") 
       then
        (SecurityModeComplete (ueCRntiValue message) True,socket)
       else (SecurityModeComplete (ueCRntiValue message) False,socket)
       where
        decrypted = decryptResponse (securityKey_ue behaviorContent) (message_security message)

    eSendMessUeCapInf :: Event t (RrcMessage, Socket) 
    eSendMessUeCapInf = createUeCapMessage <$> eMessageUeCapabilityEnquiry

    createUeCapMessage (message,socket) =
      (UECapabilityInformation  (ueCRntiValue message) (genRandomRatCapabilities ((ueCRntiValue message)*6) message) ,socket)
     where
      genRandomRatCapabilities seed message =
         
         zip ratList boolList
         where
           boolList = take 5 $ randoms (mkStdGen seed):: [Bool]
           ratList = (ueCapabilityRequest message)

    eSendMessReconfComplete :: Event t (RrcMessage, Socket) 
    eSendMessReconfComplete = createReconfCompleteMessage <$> eMessageRrcConnReconf

    createReconfCompleteMessage (message,socket) =    
      if activationComplete 
       then
        (RRCConnectionReconfigurationComplete (ueCRntiValue message) True,socket)
       else (RRCConnectionReconfigurationComplete (ueCRntiValue message) False,socket)
       where
        epsId = epsRadioBearerIdentity message
        activationComplete = not (((head epsId)== (last epsId) && ((head epsId)== '9')))--arbitrary condition
    
    eResponseMessage = eSendMessRrcCR `union` eSendMessRrcCSC `union` eSendMessSecComplete `union` eSendMessUeCapInf `union` eSendMessReconfComplete
         
  --Output
  reactimate $ sendResponse  <$> eResponseMessage
  reactimate $ writeToLog logh . showMessage <$> eMessage
  reactimate $ (finalLog_ue logh <$> bUeState)  <@> eSendMessReconfComplete 

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



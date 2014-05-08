{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Exception
import Control.Applicative
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString.Lazy

import RrcMessages
import Identifiers
import UeContextStates

import Control.Concurrent.Async

import Data.Time
import Text.Printf

import Reactive.Banana as R
import Reactive.Banana.Frameworks as R
import EventSources

import System.IO

import Control.Monad
--import Control.Monad.Trans.Loop

import System.Random

--Decryption
import Codec.Crypto.AES
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Word
import Data.Binary.Put

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
    hClose logh
    
    let diff = endTime `diffUTCTime` startTime
    printf "Time for the UEs procedures: %s\n"  (show diff)

endOfProgram ::IO()
endOfProgram =
  bracket (connectedSocket "127.0.0.1" "43000")
          close endOfProgram'
  where
    endOfProgram' :: Socket -> IO ()
    endOfProgram' sock = do
      --end message to the enodeB
      _ <- send sock $ encode (EndOfProgram)
      return ()

powerOn :: Handle -> Int -> IO ()
powerOn logh ueId =
  bracket (connectedSocket "127.0.0.1" "43000")
          close (powerOn' logh)
  where
    powerOn' :: Handle -> Socket -> IO ()
    powerOn' logh sock = do
      --create sources
      sources <- newAddHandler
      --compile and start network
      network <- compile $ setupNetwork sources ueId logh
      actuate network
      -- specific event loop
      eventLoop sources sock ueId
      --_ <- send sock $ encode (RAPreamble ueId)
      
      --randomId <- randomRIO (1, 320 :: Int)
      -- _ <- send sock $ encode (RAPreamble randomId)
      -- RAResponse _ <- decode <$> recv sock 10240
      --_ <- send sock $ encode (RRCConnectionRequest (RRCConnectionRequestIEs (STMSI ueId) Emergency 2))

      --RRCConnectionSetup _ <- decode <$> recv sock 10240
      --_ <- send sock $ encode (RRCConnectionComplete True)
      putStrLn $ "exit ue" ++ (show ueId)
      return ()

connectedSocket :: HostName -> ServiceName -> IO Socket
connectedSocket host port = do
  sock <- socket AF_INET Stream defaultProtocol
  addrInfo <- getAddrInfo Nothing (Just host) (Just port)
  let primary = head addrInfo
  connect sock $ addrAddress primary
  return sock

--Read commands and fire corresponding events (Approximation nb1)
eventLoop :: EventSource (RrcMessage,Socket)-> Socket-> Int -> IO ()
eventLoop message  sock ueId = withSocketsDo $ do
       --Send first message
       --fire message (RAPreamble 47,sock) --forkIO
       _ <- send sock $ encode (RAPreamble RA_RNTI ueId)
       --Wait for response
       loop
       where loop = do
               
       --forever $ do
         --let messDecrypt = liftIO $ (crypt CTR (B.concat (BL.toChunks(encode (ueId*18)))) (B.concat (BL.toChunks(encode (0::Int)))) Decrypt  <$> recv sock 1024)
         --messDec <- decode (crypt CTR (B.concat (BL.toChunks(encode (ueId*18)))) (B.concat (BL.toChunks(encode (0::Int)))) Decrypt  <$> recv sock 1024) --messDecrypt --(decryptResponse (ueId*18) <$> recv sock 1024)
                messDec <- decode <$> recv sock 1024
                fire message (messDec,sock) --forkIO?
                --exit the loop
                when (notlastMessage messDec) loop
                where notlastMessage mess = case mess of
                        (RRCConnectionAccept _ ) -> False
                        (RRCConnectionReject _ _) -> False
                        _ -> True
         
         --send sock $ encode (RAPreamble 45)--non execute
         
      {- putStr "> "
       hFlush stdout
       s <- getLine
       case s of
         "message1" -> fire message 1 --fire corresponding events
         "message2" -> fire message 2
         "messageI" -> fire messageI ()
         "quit" -> return ()
         _ -> putStrLn "Unknown command"
       when (s /= "quit") loop -}


{-----------------------------------------------------
     Program Logic
------------------------------------------------------}

-- Set up the event network
setupNetwork :: forall t. Frameworks t =>
   EventSource (RrcMessage,Socket) -> Int -> Handle -> Moment t ()
setupNetwork message ueId logh = do

  --Obtain events corresponding to the eNodeB
  eMessage{-:: Event t (RrcMessage,Socket) -}<- fromAddHandler (addHandler message)
  
    

  
  
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
    {-eCreateState :: Behavior t (IMSI,Int)
    eCreateState = apply createUeState eMessageRAResponse-}
    
    --State of the UE
    --bUeState :: Behavior t (IMSI,Int)
    bUeState :: Behavior t UeContext_ue
    --bUeState = R.pure <$>  (createUeState <$> bNbMessages)
    --bUeState = stepper defaultUeState (createUeState <$> eMessageRAResponse) --utiliser pure?

    {-createUeState :: (RrcMessage, Socket)-> (IMSI,Int)
    createUeState (message, _)= (genIMSI (ueRaRntiValue message)  ((ueRaRntiValue message)+2),(ueRaRntiValue message)*18)-}

    --createUeState :: Int -> (IMSI,Int)
    --createUeState seed= (genIMSI seed  (seed+2),seed*18)
    --state = (createUeState  ueId)
    --bUeState =pure state
    bUeState = stepper (defaultUeState ueId) (apply (addSrb <$> bUeState) eMessageRrcCS)

    addSrb :: UeContext_ue -> (RrcMessage,Socket) -> UeContext_ue
    addSrb oldState (message, _)= UeContext_ue{
  imsi_ue = imsi_ue oldState,
  securityKey_ue = securityKey_ue oldState,
  srbId = RrcMessages.srbIdentity message
  }
    
  --Fire a specific event depending on the type of message
  --let eMessageRAResponse ::Event t (RrcMessage,Socket)
  {-case fst eMessage of
    (RAResponse a b c, _) -> do
      let
        --eMessageRAResponse ::Event t (RrcMessage,Socket)
        eMessageRAResponse = filterE raResponse eMessage
        raResponse:: (RrcMessage, Socket)-> Bool
        raResponse x = case x of
          (RAResponse a b c, _)-> True
          _ -> False
      reactimate $ sendResponse  <$> eMessageRAResponse
      --return ()
    (RRCConnectionSetup a b c, _) -> do
      eMessageRrcCS <- filterE (\t ->True) eMessage
      reactimate $ sendResponse2  <$> eMessageRrcCS
    -}
    
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
         
      
        

    

    --Output: Is printed in the order of definition?
  --reactimate $ putStrLn . showNbMessages <$> eNbMessages
  reactimate $ putStrLn . showMessageNumber <$> eMessage
  --reactimate $ putStrLn . show <$> eMessage
    --add an output event?
  reactimate $ putStrLn . rejectMessage <$> eMessageRrcReject
  reactimate $ sendResponse  <$> eSendMessRrcCR --eMessageRAResponse
  --reactimate $ sendResponse2  <$> eMessageRAP
  --reactimate $ putStrLn "RAResponse has been identified"<$ eMessageRAResponse
  reactimate $ sendResponse  <$> eSendMessRrcCSC
  reactimate $ sendResponse <$> eSendMessSecComplete
  reactimate $ sendResponse <$> eSendMessUeCapInf
  reactimate $ sendResponse <$> eSendMessReconfComplete
  --Log
  reactimate $ writeToLog logh . showMessageNumber <$> eMessage
  reactimate $ (finalLog logh <$> bUeState)  <@> eSendMessReconfComplete 

    
showNbMessages nbMessages = "Nb of messages received in this UE: " ++ show nbMessages
showMessageNumber (number,sock) = "Message " ++ show number ++ " from eNodeB has arrived"
rejectMessage (message, _) = "RRC Connection rejected (UE C-RNTI ="++ show (ueCrnti message)++")"
--add something to really terminate the connection(kill network?)
sendResponse (message,sock) = do
                             _ <- send sock $ encode message
                             return ()

--decryptResponse :: Int ->IO(RrcMessage,Socket)-> IO ()
decryptResponse key message = 
  crypt CTR (B.concat (BL.toChunks(( BL.concat[(encode key), (encode key)]

                                                         )))) (B.concat (BL.toChunks(runPut (do
                                                                                                             putWord64be 0
                                                                                                             putWord64be 0
                                                                                                          )))) Decrypt message                  


{-sendResponse2  (number,sock) = do
                             _ <- send sock $ encode (RRCConnectionSetupComplete 4)
                             putStrLn "Sends something CS Complete"
                             return ()-}


defaultUeState :: Int -> UeContext_ue
defaultUeState seed = UeContext_ue{
  imsi_ue = genIMSI seed  (seed+2),
  securityKey_ue = seed*18,
  srbId = "0"
  }

finalLog :: Handle -> UeContext_ue -> (RrcMessage, Socket) -> IO()
finalLog handle state (message, _) = do
  writeToLog handle ("Context at the end : "++ show state)


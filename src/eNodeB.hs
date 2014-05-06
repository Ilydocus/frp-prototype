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
import Control.Exception

import qualified Data.Map.Lazy as Map
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

import Data.Binary
import Data.Binary.Put

import RrcMessages
import S1Messages --faire un as S1 ici

import Identifiers
import UeContextStates as Context

import LogTools

--Encryption test
import Codec.Crypto.AES

type ENBMap = Map.Map Int UeContext_eNB

data Message =
    RrcMessageType (RrcMessage,Socket)
  | S1APMessageType (S1APMessage,Socket)
    deriving (Eq,Show)

main :: IO ()
main = do
  sources <- makeSources 
  eNBMap <- newTVarIO Map.empty --database
  logh <- openFile "log_enb.txt" WriteMode
  --writeToLog logh "ave Test"
  --hClose logh
  network <- compile $ setupNetwork sources (return eNBMap) logh
  
  actuate network
  eventLoop sources -- pourquoi a la fin?

--Create event sources corresponding to UeMessages MMEMessages Database
makeSources = (,,) <$> newAddHandler <*> newAddHandler <*> newAddHandler

--Read commands and fire corresponding events (Approximation nb1)
eventLoop :: (EventSource (RrcMessage, Socket,Socket), EventSource (S1APMessage,Socket,Socket),EventSource (TVar ENBMap))-> IO ()
eventLoop (messageUE, messageMME, messageDatabase) = loop
   where
     loop = withSocketsDo $ do
       --eNBMap <- newTVarIO Map.empty
       listenSocket <- listenOn $ PortNumber 43000
       --(connectionSocket, _) <- accept listenSocket
       forever $ do
         --listenSocket <- listenOn $ PortNumber 43000
         (connectionSocket, _) <- accept listenSocket
         forkIO $ do
          bracket (connectedMMESocket "127.0.0.1" "43001")
                  close (powerMME connectionSocket)
          where
            powerMME :: Socket  -> Socket -> IO ()
            powerMME ueSock mmeSock = do --mettre type function
                    --first message M
             --(fire messageMME (GetSocket,mmeSock))
             --_ <- send mmeSock $ encode (S1APInitialUEMessage 12 EPSAttach "16")
             --_ <- send ueSock $ encode (RAResponse RA_RNTI 12 21)
             --putStrLn "pifpouf"
             --(fire messageDatabase (database))-- est ce que besoin une seule fois?
             --_ <- send mmeSock $ encode (S1APInitialUEMessage 15 EPSAttach "16")
             --putStrLn "pafpif"
             --forever $ do
               --putStrLn "doing stuff"
             forkIO $ do
              forever $ do
            --putStrLn "Received something on the eNodeB"
                 messDec <- decode <$> recv ueSock 1024
         
                 (fire messageUE (messDec,ueSock,mmeSock))
            --For the MME
             --forkIO $ do
             forever $  do
                 messDecMME <- decode <$> recv mmeSock 1024
                 (fire messageMME (messDecMME,ueSock,mmeSock))
                 
             return ()      
                  
      --compile and start network
         --forkIO $ (fire message (messDec,connectionSocket))
        -- putStrLn "Fired something on the eNodeB"
         
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
   (EventSource (RrcMessage,Socket,Socket), EventSource(S1APMessage, Socket,Socket), EventSource(TVar ENBMap))-> IO(TVar ENBMap) -> Handle -> Moment t ()
setupNetwork (messageUE, messageMME, messageDatabase) database logh= do

  --Obtain events corresponding to the two commands
  --eMessageUE{-:: Event t (RrcMessage,Socket)-} <- fromAddHandler (addHandler messageUE)
  --let
    --handler = addHandler messageUE
  eMessageUE{-:: Event t (RrcMessage,Socket)-} <- fromAddHandler (addHandler messageUE)
  eMessageMME <- fromAddHandler (addHandler messageMME)
  --eDatabase <- fromAddHandler (addHandler messageDatabase)--ou behavior fromChanges
  --bMessageUE <-fromChanges  eMessageUE handler
  --database <-newTVar Map.empty

  let
    
    --How to capture the state? Compare to the slot machine
    --For now, number of messages received is the state
    -- both the eMessage 1 and the emessage2 events add one to the state
    bNbMessages :: Behavior t Int
    eNbMessages :: Event t Int
    (eNbMessages, bNbMessages) = mapAccum 0 . fmap (\f x -> (f x, f x)) $
                     ((addMessage <$ eMessageUE)
                        `union` (addMessage <$ eMessageMME) )
    --Functions to change the state
    addMessage = (+ 1)

    -- State of the eNodeB is one UEContext per UE
    --database = newTVar Map.empty 
    bUeContext :: Behavior t (IO (TVar ENBMap))
    --eUeContext :: Event t (TVar ENBMap)
    --(eUeContext, bUeContext) = mapAccum database  . fmap (\f x -> (f x, f x)) $
           --(((initUEContext ) <$> eMessageRAPreamble))
           -- `union` () )
    --utiliser le stepper juste pour le debut?
    --peut etre qu'un pure pourrait marcher aussi, a voir 
    eMessageRAPreambleDone = (((initUEContext database) <$> eSendRepRAPreamble))
    bUeContext = stepper ( database) (eMessageRAPreambleDone ` union`  (addImsi database <$> eMessageRrcCR))

    --current message
    --bCurrentRrcMessage :: Behavior t (RrcMessage,Socket,Socket)
    --bCurrentRrcMessage = stepper (RAPreamble RA_RNTI 0,) (replaceCurrMess <$> eMessageRAPreamble)--change for eMess?

    --replaceCurrMess(message, _,_) = message
    
    
    --bUeContext = stepper ( database) (((initUEContext database) <$> eMessageRAPreamble))--(atomically $ newTVar Map.empty)  (((initUEContext database) <$> eMessageRAPreamble))

    -- Functions that act on the UE context
    {-initUEContext :: IO(TVar ENBMap) -> (RrcMessage, Socket,Socket) -> IO (TVar ENBMap)
    initUEContext database (message, ueSocket, mmeSocket) = do
      newData <- liftIO $ database
      atomically $ modifyTVar (newData ) (\eNBmap -> Map.insert (ueIdRntiValue message) (defaultUeContext 588) eNBmap )
      --newData <-readTVarIO database
      --return the entry that was created ?
      database-}
    initUEContext :: IO(TVar ENBMap) -> (RrcMessage, Socket) -> IO (TVar ENBMap)
    initUEContext database (message, ueSocket) = do
      newData <- liftIO $ database
      --atomically $ modifyTVar (newData ) (\eNBmap -> Map.insert (ueIdRntiValue message) (defaultUeContext (ueTempCRntiValue message)) eNBmap )
      atomically $ modifyTVar (newData ) (\eNBmap -> Map.insert (ueTempCRntiValue message) (defaultUeContext (ueTempCRntiValue message)) eNBmap )
      --newData <-readTVarIO database
      --return the entry that was created ?
      database
      
      
      --crnti <-genRandId 5 588
      --Map.adjust (modifyCrnti crnti) (ueIdRntiValue message) database
    
    --Inner functions to fire a specific event depending on the one that was received
    --one needeed per incoming message
    eMessageRAPreamble ::Event t (RrcMessage,Socket,Socket)
    eMessageRAPreamble = filterE raPreamble eMessageUE

    raPreamble:: (RrcMessage, Socket,Socket)-> Bool
    raPreamble x = case x of
      (RAPreamble m b, _,_)-> True
      _ -> False

    eMessageRrcCR ::Event t (RrcMessage,Socket,Socket)
    eMessageRrcCR = filterE rrcCR eMessageUE

    rrcCR:: (RrcMessage, Socket,Socket)-> Bool
    rrcCR x = case x of
      (RRCConnectionRequest m b c, _,_)-> True
      _ -> False

    eMessageRrcCC ::Event t (RrcMessage,Socket,Socket)
    eMessageRrcCC = filterE rrcCC eMessageUE

    rrcCC:: (RrcMessage, Socket,Socket)-> Bool
    rrcCC x = case x of
      (RRCConnectionSetupComplete m n, _,_)-> True
      _ -> False

    eMessageSecCompleted ::Event t (RrcMessage,Socket,Socket)
    eMessageSecCompleted = filterE secCompleted eMessageUE

    secCompleted:: (RrcMessage, Socket,Socket)-> Bool
    secCompleted x = case x of
      (SecurityModeComplete m n, _,_)-> True
      _ -> False

    eMessageCapInfo ::Event t (RrcMessage,Socket,Socket)
    eMessageCapInfo = filterE capInfo eMessageUE

    capInfo :: (RrcMessage, Socket,Socket)-> Bool
    capInfo x = case x of
      (UECapabilityInformation m n, _,_)-> True
      _ -> False

    eMessageReconfComplete ::Event t (RrcMessage,Socket,Socket)
    eMessageReconfComplete = filterE reconfComplete eMessageUE

    reconfComplete :: (RrcMessage, Socket,Socket)-> Bool
    reconfComplete x = case x of
      (RRCConnectionReconfigurationComplete m n, _,_)-> True
      _ -> False

    --MME messages
    eS1APInitSR ::Event t (S1APMessage,Socket,Socket)
    eS1APInitSR = filterE mmeISR eMessageMME

    mmeISR:: (S1APMessage, Socket,Socket)-> Bool
    mmeISR x = case x of
      (S1APInitialContextSetupRequest a b c d, _,_)-> True
      _ -> False  

  --Stupid MME test
    {-eMMESocket ::Event t (S1APMessage,Socket)
    eMMESocket = filterE mmeSoc eMessageMME

    mmeSoc:: (S1APMessage, Socket)-> Bool
    mmeSoc x = case x of
      (GetSocket, _)-> True
      _ -> False-}

    --responses can be sent
    --eSendRepRAPreamble :: Event t (IO(RrcMessage, Socket) )
    eSendRepRAPreamble :: Event t (RrcMessage, Socket)
    eSendRepRAPreamble = createMessageRAResponse <$>eMessageRAPreamble


  
    createMessageRAResponse (message,ueSock,_) = do
      --newBe <- liftIO behaviorContent
      --map <- readTVarIO newBe
      
       (RAResponse RA_RNTI (ueIdRntiValue message) (ueIdRntiValue message),ueSock)
       --(RAResponse RA_RNTI (ueIdRntiValue message) (genRandId 5 588),ueSock)

    eSendRepRRCConnectionRequest :: Event t (RrcMessage, Socket)
    eSendRepRRCConnectionRequest = handleConnectionRequest <$> eMessageRrcCR --apply (handleConnectionRequest <$> bUeContext)  eMessageRrcCR
    eRrcCrPart2Done = apply (addSrb <$> bUeContext)  eSendRepRRCConnectionRequest
    --eRrcCrPart1Done = apply (addImsi <$> bUeContext)  eMessageRrcCR

    handleConnectionRequest :: (RrcMessage,Socket,Socket) -> (RrcMessage,Socket) 
    handleConnectionRequest (message, ueSocket, _) = --do
      --_<-addImsiAndSrb (ueIdentity message) srbId crnti behaviorContent
      if reject then
        (RRCConnectionReject crnti ((crnti `mod` 15)+ 1),ueSocket)
        else --do
        --_<-addImsiAndSrb (ueIdentity message) srbId crnti behaviorContent
        (RRCConnectionSetup C_RNTI crnti srbId,ueSocket)
      -- Generate SRB Identity
      where
        crnti = ueIdRntiValueCR message
        srbId = genRandId 8 (crnti *4)
      --Store IMSI and SRBId in the UE Context
      
      --Decide to accept or reject
        reject = (crnti `mod` 30)== 0
      --Create appropriate message
      {-if reject then
        (RRCConnectionReject crnti ((crnti `mod` 15)+ 1),ueSocket)
        else
        (RRCConnectionSetup C_RNTI crnti srbId,ueSocket)-}
        
--a bouger peut etre dans un autre module
    addSrb :: IO(TVar ENBMap) ->(RrcMessage, Socket)->IO ()
    addSrb behaviorContent (RRCConnectionSetup _ key srb,_)= do
      tempDatabase <- liftIO behaviorContent
      map1<- readTVarIO tempDatabase
      print map1 --ok, bien modifie
      atomically $ modifyTVar tempDatabase (\ueMap -> Map.adjust (addSrb' srb) key ueMap)
      putStrLn "End:Adding srb to the state"
      print srb
      map<- readTVarIO tempDatabase
      print map --ok, bien modifie
      where 
        addSrb' srb oldContent =  UeContext_eNB {rrcState =rrcState oldContent,
  c_rnti = c_rnti oldContent,
  imsi = imsi oldContent,
  Context.srbIdentity = srb,
  eNBUES1APid =eNBUES1APid oldContent ,
  ratCapabilities =ratCapabilities oldContent,
  Context.securityKey = Context.securityKey oldContent,
  Context.epsBearerId = Context.epsBearerId oldContent }

    --addImsi :: IO(TVar ENBMap) ->(RrcMessage, Socket,Socket)->IO ()
    addImsi behaviorContent (message,_,_)= do
      putStrLn "Adding imsi to the state"
      tempDatabase <- liftIO behaviorContent
      print (ueIdentity message)
      map1<- readTVarIO tempDatabase
      print map1 --ok, bien modifie
      let
       oldCon = Map.findWithDefault (defaultEmptyContext) (ueIdRntiValueCR message) map1
      print oldCon
      atomically $ modifyTVar tempDatabase (\ueMap -> Map.insert (ueIdRntiValueCR message)(addImsi' (ueIdentity message)oldCon)  ueMap) 
      putStrLn "End:Adding imsi to the state"
      map<- readTVarIO tempDatabase
      print map --ok, bien modifie
      database
      where 
        addImsi' imsi oldContent =  UeContext_eNB {rrcState =rrcState oldContent,
  c_rnti = c_rnti oldContent,
  imsi = imsi,
  Context.srbIdentity = Context.srbIdentity oldContent,
  eNBUES1APid =eNBUES1APid oldContent ,
  ratCapabilities =ratCapabilities oldContent,
  Context.securityKey = Context.securityKey oldContent,
  Context.epsBearerId = Context.epsBearerId oldContent }
          
   
    {-eHandleCC = apply (changeRrcState <$> bUeContext)  eSendRepRrcCC
    changeRrcState :: IO(TVar ENBMap) ->(S1APMessage, Socket)->IO ()
    changeRrcState behaviorContent (message,_)= do
      tempDatabase <- liftIO behaviorContent
      atomically $ modifyTVar tempDatabase (\ueMap -> Map.adjust (changeRrcState' (eNB_UE_S1AP_Id message)) ((eNB_UE_S1AP_Id message) `div` 17) ueMap)
      where --enbtruc
        changeRrcState' enbId oldContent =  UeContext_eNB {rrcState =RRC_Connected,
  c_rnti = c_rnti oldContent,
  imsi = imsi oldContent,
  Context.srbIdentity = Context.srbIdentity oldContent,
  eNBUES1APid =enbId ,
  ratCapabilities =ratCapabilities oldContent,
  Context.securityKey = Context.securityKey oldContent,
  Context.epsBearerId = Context.epsBearerId oldContent }-}

      --(apply ((createMessage <$> bUeContext)) eMessageRAPreamble )--(apply (returnMess<$> bMessageUE) eMessageRAPreambleDone) ) -- <$ eMessageRAPreambleDone

    {-createMessage behaviorContent (message,ueSock,_) = do
      newBe <- liftIO behaviorContent
      map <- readTVarIO newBe
      return (RAResponse RA_RNTI (ueIdRntiValue message) (c_rnti (Map.findWithDefault defaultEmptyContext (ueIdRntiValue message) (map))),ueSock)-}

    --returnMess behavior _ = behavior


    eSendRepRrcCC :: Event t (IO(S1APMessage, Socket))
    eSendRepRrcCC = apply (createMessageCCResponse <$> bUeContext) eMessageRrcCC


  
    createMessageCCResponse behaviorContent (message,_,mmeSocket) = do
     --newBe <- liftIO behaviorContent
     tempDatabase <- liftIO behaviorContent
     atomically $ modifyTVar tempDatabase (\ueMap -> Map.adjust (changeRrcState' enbUeId) (ueCRntiValue message) ueMap)
      
     map <- readTVarIO tempDatabase
     print map 
     return(S1APInitialUEMessage enbUeId EPSAttach (imsi (Map.findWithDefault defaultEmptyContext (ueCRntiValue message) ((map)))),mmeSocket)
     where --enbtruc changer le nom !xs
        changeRrcState' enbId oldContent =  UeContext_eNB {rrcState =RRC_Connected,
  c_rnti = c_rnti oldContent,
  imsi = imsi oldContent,
  Context.srbIdentity = Context.srbIdentity oldContent,
  eNBUES1APid =enbId ,
  ratCapabilities =ratCapabilities oldContent,
  Context.securityKey = Context.securityKey oldContent,
  Context.epsBearerId = Context.epsBearerId oldContent }
        enbUeId= 17*(ueCRntiValue message)
     {-where
       extractMap behavior = do
         newBe <- liftIO behavior
         map <- readTVar newBe
         return map
         --liftIO behavior-}
               
    eSendRepInitCSR :: Event t (IO(RrcMessage, Socket))
    eSendRepInitCSR = apply (createMessageInitCSRResponse <$> bUeContext) eS1APInitSR

    createMessageInitCSRResponse :: IO(TVar ENBMap)-> (S1APMessage,Socket,Socket)-> IO(RrcMessage,Socket)
    createMessageInitCSRResponse behaviorContent (message,ueSocket,_) = do
     tempDatabase <- liftIO behaviorContent
     atomically $ modifyTVar tempDatabase (\ueMap -> Map.adjust (changeSecurityKey' (S1Messages.securityKey message) (S1Messages.epsBearerId message)) ((eNB_UE_S1AP_Id message)`div`17) ueMap)
      
     map <- readTVarIO tempDatabase
     print map 
     --let crnti = (eNB_UE_S1AP_Id message)`div`17
     return(SecurityModeCommand ((eNB_UE_S1AP_Id message)`div`17) (encryptString (S1Messages.securityKey message) "ciphered") ,ueSocket)
     where --add eps in the name
        changeSecurityKey' key eps oldContent =  UeContext_eNB {rrcState =RRC_Connected,
  c_rnti = c_rnti oldContent,
  imsi = imsi oldContent,
  Context.srbIdentity = Context.srbIdentity oldContent,
  eNBUES1APid =eNBUES1APid oldContent ,
  ratCapabilities =ratCapabilities oldContent,
  Context.securityKey = key,
  Context.epsBearerId = eps }
       -- enbUeId= 17*(ueCRntiValue message)
  --type UeContext_eNB
    --dummy be (message,_,_) =(Map.findWithDefault defaultEmptyContext (ueIdRntiValue message) (liftIO $ readTVarIO be))
    --type String
    --dummy be (message,_,_) =c_rnti (Map.findWithDefault defaultEmptyContext (ueIdRntiValue message) (liftIO $ readTVarIO be))
    --type RRCMessahe
    --dummy be (message,sock,_) =RAResponse RA_RNTI (ueIdRntiValue message) (c_rnti (Map.findWithDefault defaultEmptyContext (ueIdRntiValue message) (liftIO $ readTVarIO be)))
    --dummy be (message,sock,_) =(RAResponse RA_RNTI 12 "15",sock)
    --dummy ::(TVar ENBMap) -> (RrcMessage, Socket, Socket)-> (RrcMessage,Socket)
    --dummy be (message,sock,_) =(RAResponse RA_RNTI (ueIdRntiValue message) (c_rnti (Map.findWithDefault defaultEmptyContext (ueIdRntiValue message) (liftIO $ readTVarIO be))),sock)                                                                              --dummy2 :: RrcMessage -> (RrcMessage, Socket, Socket) -> (RrcMessage,Socket)
    --dummy2 message (_,sock,_)= (message, sock)
        



      {-do
                                        var <-liftIO $ (readTVarIO <$>behaviorContent)
                                        return (RAResponse RA_RNTI (ueIdRntiValue message) (c_rnti (Map.findWithDefault defaultEmptyContext (ueIdRntiValue message) var)))
-}
    {-mmeSocket :: Behavior t Socket
    eMmeSocket :: Event t Socket
    eMmeSocket = (extractSocket <$> eMMESocket)-- a rassembler ces  2 lignes?
    mmeSocket = stepper dummySocket eMmeSocket

    dummySocket::Socket
    {-dummySocket =  do
      sock<-liftIO (socket AF_INET Stream defaultProtocol)
      return sock-}
                   
    {-(eMmeSocket,mmeSocket) = mapAccum 0 . fmap (\f x -> (f x, f x)) $(extractSocket <$> eMMESocket)-}

    extractSocket:: (S1APMessage, Socket) -> Socket
    extractSocket (message,socket)= socket-}
 
  --case eMessage of
  -- (RAPreamble m, Socket s) -> eMessageRAPreamble m s
    eSendRepSecurityComplete :: Event t (IO(RrcMessage, Socket))
    eSendRepSecurityComplete = apply (handleSecurityComplete <$> bUeContext) eMessageSecCompleted --passer un apply ici 

    handleSecurityComplete :: IO(TVar ENBMap)->(RrcMessage,Socket,Socket) -> IO(RrcMessage,Socket) 
    handleSecurityComplete behaviorContent (message, ueSocket, _) = 
      if (securityModeSuccess message) then do
        return (UECapabilityEnquiry crnti [E_UTRA,UTRA,GERAN_CS,GERAN_PS,CDMA2000],ueSocket)
        else do
         _ <- changeStateToIdle crnti behaviorContent
         return (RRCConnectionReject crnti ((crnti `mod` 15)+ 1),ueSocket)
      where
        crnti = ueCRntiValue message
        
--AFAIRE: FONCTION POUR REPASSER EN RRC IDLE
    changeStateToIdle key behaviorContent = do
     tempDatabase <- liftIO behaviorContent
     atomically $ modifyTVar tempDatabase (\ueMap -> Map.adjust changeStateToIdle' key ueMap)
     --Just to check
     map <- readTVarIO tempDatabase
     print map
     where 
        changeStateToIdle' oldContent =  UeContext_eNB {
  rrcState =RRC_Idle,
  c_rnti = c_rnti oldContent,
  imsi = imsi oldContent,
  Context.srbIdentity = Context.srbIdentity oldContent,
  eNBUES1APid =eNBUES1APid oldContent ,
  ratCapabilities =ratCapabilities oldContent,
  Context.securityKey = Context.securityKey oldContent,
  Context.epsBearerId = Context.epsBearerId oldContent}

    eSendRepCapInfo :: Event t (IO(RrcMessage, Socket))
    eSendRepCapInfo = apply (createMessageCapInfoResponse <$> bUeContext) eMessageCapInfo
    
    createMessageCapInfoResponse :: IO(TVar ENBMap)-> (RrcMessage,Socket,Socket)-> IO(RrcMessage,Socket)
    createMessageCapInfoResponse behaviorContent (message,ueSocket,_) = do
     tempDatabase <- liftIO behaviorContent
     atomically $ modifyTVar tempDatabase (\ueMap -> Map.adjust (storeRATCapabilities' (ueCapabilityRatList message)) (ueCRntiValue message) ueMap)
     map <- readTVarIO tempDatabase
--just to check
     print map 

     return (RRCConnectionReconfiguration (ueCRntiValue message) (Context.epsBearerId (Map.findWithDefault defaultEmptyContext (ueCRntiValue message) (map))) ,ueSocket)
     where
        storeRATCapabilities' ratList oldContent =  UeContext_eNB {rrcState =rrcState oldContent,
  c_rnti = c_rnti oldContent,
  imsi = imsi oldContent,
  Context.srbIdentity = Context.srbIdentity oldContent,
  eNBUES1APid =eNBUES1APid oldContent ,
  ratCapabilities =ratList,
  Context.securityKey = Context.securityKey oldContent,
  Context.epsBearerId = Context.epsBearerId oldContent }

    eSendRepReconfComplete :: Event t (IO Message)
    eSendRepReconfComplete = apply (handleReconfComplete <$> bUeContext) eMessageReconfComplete 

    handleReconfComplete :: IO(TVar ENBMap)->(RrcMessage,Socket,Socket) -> IO(Message) 
    handleReconfComplete behaviorContent (message, ueSocket, mmeSocket) = 
      if (epsRadioBearerActivated message) then do
        enbId <- liftIO $ getEnbId behaviorContent crnti
        return (S1APMessageType(S1APInitialContextSetupResponse enbId (genRandId 5 (enbId +45)),mmeSocket))
        else do
         _ <- changeStateToIdle crnti behaviorContent
         return (RrcMessageType(RRCConnectionReject crnti ((crnti `mod` 15)+ 1),ueSocket))
      where
        crnti = ueCRntiValue message
        
        
        getEnbId behaviorContent key= do
              tempDatabase <- liftIO behaviorContent
              map <- readTVarIO tempDatabase
              return (eNBUES1APid (Map.findWithDefault defaultEmptyContext key map))
       

    --Output: Is printed in the order of definition?
  reactimate $ putStrLn . showNbMessages <$> eNbMessages
  reactimate $ putStrLn . showMessageNumber <$> eMessageUE
  reactimate $ putStrLn . showMessageNumberMME <$> eMessageMME
  --reactimate $ sendResponse (RAResponse RA_RNTI 12 21) <$> eMessageRAPreamble
  reactimate $ sendResponseToUe <$> eSendRepRAPreamble -- <$> eMessageRAPreamble
  --reactimate $ sendResponseMME (S1APInitialUEMessage 13 EPSAttach "16") <$> eMessageRAPreamble
  --reactimate $ sendResponse (RRCConnectionSetup C_RNTI 45 54) <$> eMessageRrcCR
  reactimate $ sendResponseToUe <$> eSendRepRRCConnectionRequest
  reactimate $ putStrLn "CR received " <$ eMessageRrcCR
  --reactimate $ sendResponse (RRCConnectii True) <$> eMessageRrcCC
  reactimate $ putStrLn  "ConnectionSetup is complete " <$ eMessageRrcCC

  --test du MME
  reactimate $ sendResponseMME  <$>eSendRepRrcCC
  --reactimate $ apply (addImsi <$> bUeContext)  eMessageRrcCR
  reactimate $ apply (addSrb <$> bUeContext)  eSendRepRRCConnectionRequest
  --reactimate $ apply (sendResponseEncrypted <$>bUeContext) eSendRepInitCSR
  reactimate $ sendResponseToUeIO <$> eSendRepInitCSR
  reactimate $ sendResponseToUeIO <$> eSendRepSecurityComplete
  reactimate $ sendResponseToUeIO <$> eSendRepCapInfo
  reactimate $ sendResponseToReconf <$>  eSendRepReconfComplete
--CHANGER NOM DE SENDRESTOUEIO CAR ON PEUT ENVOYER AU MME AUSSI AVEC
    --sock <- createSocketThingy <$>eMMESocket
    --(sendResponseMME (S1APInitialUEMessage 12 EPSAttach "16")(extractSocket<$>eMMESocket)) <$ eMessageRrcCC
    --(createSocketThingy (S1APInitialUEMessage 12 EPSAttach "16")<$> eMMESocket <$ eMessageRrcCC)
  --reactimate $ putStrLn  "MME Socket detected " <$ eMMESocket
    --add an output event?
  --Log file
  reactimate $ writeToLog logh . showMessageNumber <$> eMessageUE
  --reactimate $ hClose logh <$  eSendRepReconfComplete
  reactimate $ (finalLog logh <$> bUeContext)  <@> eMessageReconfComplete --applyibfix

    
showNbMessages nbMessages = "Nb of messages received: " ++ show nbMessages
showMessageNumber (number, socket,mmesocket) = "Message from UE " ++ show number ++ " has arrived"
sendResponse message (number,sock,mmeSocket) = do
                             _ <- send sock $ encode message
                             return ()
--sendResponseToRAPreamble :: IO (RrcMessage,Socket)-> IO ()
sendResponseToUe ::  (RrcMessage,Socket)-> IO ()
sendResponseToUe (message, ueSocket) = do
                             --(message, ueSocket)<- x
                              
                             --let message= (RAResponse RA_RNTI (ueIdRntiValue message) (c_rnti (Map.findWithDefault 0 (ueIdRntiValue message)    <$> ( bUeContext))) )
                             
                             _ <- send ueSocket $ encode message --(createMessage message <$> bUeContext)--messageNew
                             return ()

sendResponseToReconf ::  IO Message-> IO ()
sendResponseToReconf x = do
  messageComp<- liftIO x
  case messageComp of
    RrcMessageType (message,ueSock) -> do
                                         _ <- send ueSock $ encode message
                                         return ()
    S1APMessageType (message,mmeSock) -> do
          _ <- send mmeSock $ encode message
          return ()
    
                             

{-sendResponseToRAPreamble x = do
                             (message, ueSocket)<- x
                              
                             --let message= (RAResponse RA_RNTI (ueIdRntiValue message) (c_rnti (Map.findWithDefault 0 (ueIdRntiValue message)    <$> ( bUeContext))) )
                             
                             _ <- send ueSocket $ encode message --(createMessage message <$> bUeContext)--messageNew
                             return ()-}


{-createMessage message behaviorContent = do
                                        var <-liftIO $ (readTVarIO <$>behaviorContent)
                                        return (RAResponse RA_RNTI (ueIdRntiValue message) (c_rnti (Map.findWithDefault defaultEmptyContext (ueIdRntiValue message) var)))-}
                                        
showMessageNumberMME (number,_ ,mmesocket) = "Message from MME " ++ show number ++ " has arrived"

sendResponseMME ::IO(S1APMessage,Socket)-> IO()
sendResponseMME x = do
                             --stupid stuff for the dummy
  (message, mmeSock)<- liftIO x
  putStrLn "Trying to send to the mme"
  --(messageTemp,mmeSockTemp) <- liftIO (message, mmesock)   
  _ <- send mmeSock $ encode message
  return ()

sendResponseToUeIO ::IO(RrcMessage,Socket)-> IO()
sendResponseToUeIO x = do
  (message, ueSock)<- liftIO x
  _ <- send ueSock $ encode message
  return ()

sendResponseEncrypted :: IO(TVar ENBMap) ->IO(RrcMessage,Socket)-> IO ()
sendResponseEncrypted behaviorContent x = do
  (message, ueSocket) <- liftIO x
  tempDatabase <- liftIO behaviorContent      
  map <- readTVarIO tempDatabase
  let
    key = Context.securityKey(Map.findWithDefault defaultEmptyContext (ueCRntiValue message) map)
    codedMessage = crypt CTR (B.concat (BL.toChunks(encode key))) (B.concat (BL.toChunks(encode (0::Int)))) Encrypt (encode message)                                                         
  _ <- send ueSocket $ codedMessage
  return ()

encryptString key message = crypt CTR (B.concat (BL.toChunks( BL.concat[(encode key), (encode key)]

                                                         ))) (B.concat (BL.toChunks(runPut (do
                                                                                                             putWord64be 0
                                                                                                             putWord64be 0
                                                                                                          )))) Encrypt (encode message)        
  
{-createSocketThingy message event rrcmess  = do
  
    --mmeSocket :: Behavior t Socket
    --eMmeSocket :: Event t Socket
    eMmeSocket <- (extractSocket <$> event)-- a rassembler ces  2 lignes?
    _ <- send eMmeSocket $ encode message
    return ()
    --mmeSocket <- stepper dummySocket eMmeSocket
    --return ()
    where
       extractSocket:: (S1APMessage, Socket) -> Socket
       extractSocket (message,socket)= socket-}
   
       {-dummySocket::Socket
       dummySocket =  do
        sock<-socket AF_INET Stream defaultProtocol
        return sock-}
                   
    {-(eMmeSocket,mmeSocket) = mapAccum 0 . fmap (\f x -> (f x, f x)) $(extractSocket <$> eMMESocket)-}
--extractSocket:: (S1APMessage, Socket) -> Socket
 --      extractSocket (message,socket)= socket
   
connectedMMESocket :: HostName -> ServiceName -> IO Socket
connectedMMESocket host port = do
  sock <- socket AF_INET Stream defaultProtocol
  addrInfo <- getAddrInfo Nothing (Just host) (Just port)
  let primary = head addrInfo
  connect sock $ addrAddress primary
  return sock

defaultUeContext :: Int -> UeContext_eNB
defaultUeContext crnti = UeContext_eNB {rrcState = RRC_Idle,
  c_rnti = crnti,
  imsi = IMSI "0" "0" "0",
  Context.srbIdentity = "0",
  eNBUES1APid = 0,
  ratCapabilities =[(E_UTRA,False)],--default RAT a revoir
  Context.securityKey = 0,
  Context.epsBearerId = "0" }

{-modifyCrnti crnti context = 
  --recuperer le vieux context et en creer un autre
  UeContext_eNB {rrcState = (rrcState context),
  c_rnti = crnti,
  imsi = (imsi context),
  Context.srbIdentity = (Context.srbIdentity context),
  eNBUES1APid = (eNBUES1APid context),
  ratCapabilities =(ratCapabilities context),
  Context.securityKey = (Context.securityKey context),
  Context.epsBearerId = (Context.epsBearerId context)}-}

defaultEmptyContext :: UeContext_eNB
defaultEmptyContext = UeContext_eNB {rrcState = RRC_Idle,
  c_rnti = -1,
  imsi = IMSI "0" "0" "0",
  Context.srbIdentity = "0",
  eNBUES1APid = 0,
  ratCapabilities =[(E_UTRA,False)],--default RAT a revoir
  Context.securityKey = 0,
  Context.epsBearerId = "0" }

finalLog :: Handle -> IO(TVar ENBMap) -> (RrcMessage, Socket, Socket) -> IO()
finalLog handle behaviorContent (message, _, _) = do
  tempDatabase <- liftIO behaviorContent
  map <- readTVarIO tempDatabase
  let
    key = ueCRntiValue message
    --lastContext :: UeContext_eNB
    lastContext = Map.findWithDefault defaultEmptyContext key map
  writeToLog handle ("Context at the end : "++ show lastContext)
  hClose handle

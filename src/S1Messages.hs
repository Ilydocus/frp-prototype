{-# LANGUAGE DeriveGeneric #-}
module S1Messages
       ( S1APMessage (..)
       , encode
       , decode
       , EPSAttach (..)
       , S1MessageType (..)
       , LastMessage (..)  
       ) where

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get 
import GHC.Generics
import qualified Data.ByteString as BS
import Identifiers
import Network.Socket
import RrcMessages

data S1APMessage =
    S1APInitialUEMessage {eNB_UE_S1AP_Id :: !Int
                         ,epsAttachType :: EPSAttach
                         ,identity :: !IMSI}
  | S1APInitialContextSetupRequest {mme_UE_S1AP_Id :: !Int
                                   ,eNB_UE_S1AP_Id :: !Int
                                   ,securityKey :: !Int
                                   ,epsBearerId :: !String}
  | S1APInitialContextSetupResponse {eNB_UE_S1AP_Id :: !Int
                                    ,eRabId :: !String}
  | EndOfProgramMME   
  deriving (Eq, Generic, Show)

instance Binary S1APMessage
         where
           put m = do
                    --Add a message type prefix 
                    case m of
                     S1APInitialUEMessage a b c -> do
                                     putWord8 0
                                     put a
                                     put b
                                     put c
                     S1APInitialContextSetupRequest a b c d -> do
                                     putWord8 1
                                     put a
                                     put b
                                     put c
                                     put d
                     S1APInitialContextSetupResponse a b -> do
                                     putWord8 2
                                     put a
                                     put b
                     EndOfProgramMME -> putWord8 3
           
           get = do id<- getWord8
                    case id of
                      0 ->do
                        a<-get
                        b<-get
                        c<-get
                        return (S1APInitialUEMessage a b c)
                      1 -> do
                        a<-get
                        b<-get
                        c<-get
                        d<-get
                        return (S1APInitialContextSetupRequest a b c d)
                      2 -> do
                        a<-get
                        b<-get
                        return (S1APInitialContextSetupResponse a b)
                      3 -> return (EndOfProgramMME)
                      

--Enumerated Data Types

data EPSAttach =
      EPSAttach
    | EPSOther
    deriving (Eq, Generic, Show)
             
instance Binary EPSAttach
   where put m = do
                  case m of
                   EPSAttach -> do
                    putWord8 0
                   EPSOther -> do
                    putWord8 1
         get = do id<- getWord8
                  case id of
                      0 ->do
                        return EPSAttach
                      1 -> do
                        return EPSOther
                        
--Type for filtering the events 
data S1MessageType =
    S1ApIUeM
  | S1ApICSRequest
  | S1ApICSResponse
  | EndProgMme
    deriving (Eq, Show)

--Specific type for the last message of the program
data LastMessage =
    ReconfigurationFailed (RrcMessage,Socket)
  | ReconfigurationCompleted (S1APMessage,Socket,RrcMessage, Socket)
    deriving (Eq,Show)

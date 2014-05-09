{-# LANGUAGE DeriveGeneric #-}
module RrcMessages
       ( RrcMessage (..)
       , encode
       , decode
       , UeIdRntiType (..)
       , RAT (..)
       , RrcMessageType (..)  
       ) where

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import GHC.Generics
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Identifiers

data RrcMessage =
    RAPreamble {ueIdRntiType :: UeIdRntiType
               ,ueIdRntiValue :: !Int}
  | RAResponse {ueIdRntiType :: UeIdRntiType
               ,ueRaRntiValue :: !Int
               ,ueIdCRnti :: !Int}
  | RRCConnectionRequest {ueIdRntiType :: UeIdRntiType
                          ,ueIdRntiValue :: !Int
                          ,ueIdentity :: !IMSI}
  | RRCConnectionReject {ueCRnti :: !Int
                        ,waitingTime :: !Int}
  | RRCConnectionAccept {ueCRnti :: !Int}
  | RRCConnectionSetup {ueIdRntiType :: UeIdRntiType
                        ,ueIdRntiValue :: !Int
                        ,srbIdentity :: !String}
  | RRCConnectionSetupComplete {ueCRnti :: !Int,
                                selectedPlmnIdentity :: !String}
  | SecurityModeCommand {ueCRnti :: !Int
                        ,message_security :: BL.ByteString}
  | SecurityModeComplete {ueCRnti :: !Int
                         ,securityModeSuccess :: !Bool}
  | UECapabilityEnquiry {ueCRnti :: !Int
                        ,ueCapabilityRequest :: [RAT]}
  | UECapabilityInformation {ueCRnti :: !Int
                            ,ueCapabilityRatList :: [(RAT,Bool)]}
  | RRCConnectionReconfiguration {ueCRnti :: !Int
                                 ,epsRadioBearerIdentity :: !String}
  | RRCConnectionReconfigurationComplete {ueCRnti :: !Int
                                         ,epsRadioBearerActivated :: !Bool}
  |EndOfProgram
  deriving (Eq, Generic, Show)

instance Binary RrcMessage
         where
           put m = do
                    --Add a message type prefix
                    case m of
                     RAPreamble a b -> do
                                     putWord8 0
                                     put a
                                     put b
                     RAResponse a b c -> do
                                     putWord8 1
                                     put a
                                     put b
                                     put c
                     RRCConnectionRequest a b c -> do
                                     putWord8 2
                                     put a
                                     put b
                                     put c
                     RRCConnectionSetup a b c -> do
                                     putWord8 3
                                     put a
                                     put b
                                     put c
                     RRCConnectionSetupComplete a b -> do
                                     putWord8 4
                                     put a
                                     put b
                     SecurityModeCommand a b-> do
                                     putWord8 7
                                     put a
                                     put b
                     SecurityModeComplete a b -> do
                                     putWord8 8
                                     put a
                                     put b
                     UECapabilityEnquiry a b -> do
                                     putWord8 9
                                     put a
                                     put b
                     UECapabilityInformation a b -> do
                                     putWord8 10
                                     put a
                                     put b
                     RRCConnectionReconfiguration a b -> do
                                     putWord8 11
                                     put a
                                     put b
                     RRCConnectionReconfigurationComplete a b -> do
                                     putWord8 12
                                     put a
                                     put b
                     RRCConnectionReject a b -> do
                                     putWord8 14
                                     put a
                                     put b
                     RRCConnectionAccept a -> do
                                     putWord8 15
                                     put a
                     EndOfProgram -> putWord8 16

           get = do id<- getWord8
                    case id of
                      0 ->do
                        a<-get
                        b<-get
                        return (RAPreamble a b)
                      1 -> do
                        a<-get
                        b<-get
                        c<-get
                        return (RAResponse a b c)
                      2 -> do
                        a<-get
                        b<-get
                        c<-get
                        return (RRCConnectionRequest a b c)
                      3 -> do
                        a<-get
                        b<-get
                        c<-get
                        return (RRCConnectionSetup a b c)
                      4 -> do
                        a<-get
                        b<-get
                        return (RRCConnectionSetupComplete a b)
                      7 -> do
                        a<-get
                        b<-get
                        return (SecurityModeCommand a b)
                      8 -> do
                        a<-get
                        b<-get
                        return (SecurityModeComplete a b)
                      9 -> do
                        a<-get
                        b<-get
                        return (UECapabilityEnquiry a b)
                      10 -> do
                        a<-get
                        b<-get
                        return (UECapabilityInformation a b)
                      11 -> do
                        a<-get
                        b<-get
                        return (RRCConnectionReconfiguration a b)
                      12 -> do
                        a<-get
                        b<-get
                        return (RRCConnectionReconfigurationComplete a b)
                      14 -> do
                        a<-get
                        b<-get
                        return (RRCConnectionReject a b)
                      15 -> do
                        a<-get
                        return (RRCConnectionAccept a)
                      16 -> return (EndOfProgram)

--Enumerated Data Types

data UeIdRntiType =
    RA_RNTI
  | C_RNTI
    deriving (Eq, Generic, Show)
             
instance Binary UeIdRntiType
   where put m = do
                  case m of
                   RA_RNTI -> do
                    putWord8 0
                   C_RNTI -> do
                    putWord8 1
         get = do id<- getWord8
                  case id of
                      0 ->do
                        return RA_RNTI
                      1 -> do
                        return C_RNTI 

data RAT =
    E_UTRA
  | UTRA
  | GERAN_CS
  | GERAN_PS
  | CDMA2000
    deriving (Eq, Generic, Show)

instance Binary RAT
   where put m = do
                  case m of
                   E_UTRA -> do
                    putWord8 0
                   UTRA -> do
                    putWord8 1
                   GERAN_CS -> do
                    putWord8 2
                   GERAN_PS -> do
                    putWord8 3
                   CDMA2000 -> do
                    putWord8 4 
         get = do id<- getWord8
                  case id of
                      0 ->do
                        return E_UTRA
                      1 -> do
                        return UTRA
                      2 -> do
                        return GERAN_CS
                      3 -> do
                        return GERAN_PS
                      4 -> do
                        return CDMA2000 

--To identify the incoming messages
data RrcMessageType =
    RaP
  | RaR
  | RrcCRequest
  | RrcCS
  | RrcCSC
  | SecurityMCommand
  | SecurityMComplete
  | UeCE
  | UeCI
  | RrcCReconfiguration
  | RrcCRC
  | RrcCReject
  | RrcCA  
  | EndProgEnb  
    deriving (Eq,Show)
    

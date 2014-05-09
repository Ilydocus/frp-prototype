{-# LANGUAGE DeriveGeneric #-}
module RrcMessages
       ( RrcMessage (..)
       , encode
       , decode
       , UeIdRntiType (..)
       , Rat (..)
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
    RaPreamble {ueIdRntiType :: UeIdRntiType
               ,ueIdRntiValue :: !Int}
  | RaResponse {ueIdRntiType :: UeIdRntiType
               ,ueRaRntiValue :: !Int
               ,ueIdCRnti :: !Int}
  | RrcConnectionRequest {ueIdRntiType :: UeIdRntiType
                          ,ueIdRntiValue :: !Int
                          ,ueIdentity :: !Imsi}
  | RrcConnectionReject {ueCRnti :: !Int
                        ,waitingTime :: !Int}
  | RrcConnectionAccept {ueCRnti :: !Int}
  | RrcConnectionSetup {ueIdRntiType :: UeIdRntiType
                        ,ueIdRntiValue :: !Int
                        ,srbIdentity :: !String}
  | RrcConnectionSetupComplete {ueCRnti :: !Int,
                                selectedPlmnIdentity :: !String}
  | SecurityModeCommand {ueCRnti :: !Int
                        ,message_security :: BL.ByteString}
  | SecurityModeComplete {ueCRnti :: !Int
                         ,securityModeSuccess :: !Bool}
  | UeCapabilityEnquiry {ueCRnti :: !Int
                        ,ueCapabilityRequest :: [Rat]}
  | UeCapabilityInformation {ueCRnti :: !Int
                            ,ueCapabilityRatList :: [(Rat,Bool)]}
  | RrcConnectionReconfiguration {ueCRnti :: !Int
                                 ,epsRadioBearerIdentity :: !String}
  | RrcConnectionReconfigurationComplete {ueCRnti :: !Int
                                         ,epsRadioBearerActivated :: !Bool}
  |EndOfProgramEnb
  deriving (Eq, Generic, Show)

instance Binary RrcMessage
         where
           put m = do
                    --Add a message type prefix
                    case m of
                     RaPreamble a b -> do
                                     putWord8 0
                                     put a
                                     put b
                     RaResponse a b c -> do
                                     putWord8 1
                                     put a
                                     put b
                                     put c
                     RrcConnectionRequest a b c -> do
                                     putWord8 2
                                     put a
                                     put b
                                     put c
                     RrcConnectionSetup a b c -> do
                                     putWord8 3
                                     put a
                                     put b
                                     put c
                     RrcConnectionSetupComplete a b -> do
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
                     UeCapabilityEnquiry a b -> do
                                     putWord8 9
                                     put a
                                     put b
                     UeCapabilityInformation a b -> do
                                     putWord8 10
                                     put a
                                     put b
                     RrcConnectionReconfiguration a b -> do
                                     putWord8 11
                                     put a
                                     put b
                     RrcConnectionReconfigurationComplete a b -> do
                                     putWord8 12
                                     put a
                                     put b
                     RrcConnectionReject a b -> do
                                     putWord8 14
                                     put a
                                     put b
                     RrcConnectionAccept a -> do
                                     putWord8 15
                                     put a
                     EndOfProgramEnb -> putWord8 16

           get = do id<- getWord8
                    case id of
                      0 ->do
                        a<-get
                        b<-get
                        return (RaPreamble a b)
                      1 -> do
                        a<-get
                        b<-get
                        c<-get
                        return (RaResponse a b c)
                      2 -> do
                        a<-get
                        b<-get
                        c<-get
                        return (RrcConnectionRequest a b c)
                      3 -> do
                        a<-get
                        b<-get
                        c<-get
                        return (RrcConnectionSetup a b c)
                      4 -> do
                        a<-get
                        b<-get
                        return (RrcConnectionSetupComplete a b)
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
                        return (UeCapabilityEnquiry a b)
                      10 -> do
                        a<-get
                        b<-get
                        return (UeCapabilityInformation a b)
                      11 -> do
                        a<-get
                        b<-get
                        return (RrcConnectionReconfiguration a b)
                      12 -> do
                        a<-get
                        b<-get
                        return (RrcConnectionReconfigurationComplete a b)
                      14 -> do
                        a<-get
                        b<-get
                        return (RrcConnectionReject a b)
                      15 -> do
                        a<-get
                        return (RrcConnectionAccept a)
                      16 -> return (EndOfProgramEnb)

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

data Rat =
    E_UTRA
  | UTRA
  | GERAN_CS
  | GERAN_PS
  | CDMA2000
    deriving (Eq, Generic, Show)

instance Binary Rat
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
    

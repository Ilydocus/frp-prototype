{-# LANGUAGE DeriveGeneric #-}
module RrcMessages
       ( RrcMessage (..)
       , encode
       , decode
       , UeIdRntiType (..)
       , RAT (..)
       , DedicatedInfoType (..)  
       ) where

import Data.Binary
import Data.Binary.Put --added cause of error message
import Data.Binary.Get --same
import GHC.Generics
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Identifiers

data RrcMessage =
    RAPreamble {ueIdRntiType :: UeIdRntiType
               ,ueIdRntiValue :: !Int}
  | RAResponse {ueIdRntiType :: UeIdRntiType
               ,ueRaRntiValue :: !Int
               ,ueTempCRntiValue :: !Int}
  | RRCConnectionRequest {ueIdRntiType :: UeIdRntiType
               ,ueIdRntiValueCR :: !Int
               ,ueIdentity :: !IMSI}
  | RRCConnectionReject {ueCrnti :: !Int
                        ,waitingTime :: !Int}
  | RRCConnectionAccept {ueCrnti :: !Int}
  | RRCConnectionSetup {ueIdRntiType :: UeIdRntiType
               ,ueIdRntiValue :: !Int
               ,srbIdentity :: !String}
  | RRCConnectionSetupComplete {ueCRntiValue :: !Int,
      selectedPlmnIdentity :: !String}
  | SecurityModeCommand {ueCRntiValue :: !Int
                         ,message_security :: BL.ByteString}
  | SecurityModeComplete {ueCRntiValue :: !Int
                          ,securityModeSuccess :: !Bool}
  | UECapabilityEnquiry {ueCRntiValue :: !Int
                         ,ueCapabilityRequest :: [RAT]}
  | UECapabilityInformation {ueCRntiValue :: !Int
                             ,ueCapabilityRatList :: [(RAT,Bool)]}
  | RRCConnectionReconfiguration {ueCRntiValue :: !Int
                                  ,epsRadioBearerIdentity :: !String}
  | RRCConnectionReconfigurationComplete {ueCRntiValue :: !Int
                                          ,epsRadioBearerActivated :: !Bool}
  | UplinkInformationTransfer {dedicatedInfoType :: DedicatedInfoType
                              ,message :: !String }
  |EndOfProgram
  deriving (Eq, Generic, Show)--derived show

instance Binary RrcMessage
         where
           put m = do
                    --Add a message type prefix to the message
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
                     UplinkInformationTransfer a b -> do
                                     putWord8 16
                                     put a
                                     put b
                     RRCConnectionReject a b -> do
                                     putWord8 14
                                     put a
                                     put b
                     RRCConnectionAccept a -> do
                                     putWord8 15
                                     put a
                     EndOfProgram -> putWord8 17
                     

                    
           {-put (RAPreamble m) = do
             putWord8 0 --differentiate the messages
             put m
           put (RAResponse b) = do
             putWord8 1
             put b
           put (RRCConnectionRequest b) = do
             putWord8 2
             put b
           put (RRCConnectionSetup b) = do
             putWord8 3
             put b
           put (RRCConnectionComplete b) = do
             putWord8 4
             put b-}
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
                      16 -> do
                        a<-get
                        b<-get
                        return (UplinkInformationTransfer a b)
                      14 -> do
                        a<-get
                        b<-get
                        return (RRCConnectionReject a b)
                      15 -> do
                        a<-get
                        return (RRCConnectionAccept a)
                      17 -> return (EndOfProgram)
                 
                         {-get::(RAResponse i) = do i<- get
                                  return (RAResponse i)-}
                    {-case i of
                      RAPreamble m -> return (RAPreamble m)
                      RAResponse b -> return (RAResponse b)-}
                     --need to be added to the other instance binary, ask why
          
                    
{-instance Show RrcMessage where
  show (RAPreamble m) = "RAPreamble "++show m
  show (RAResponse b) = "RAResponse "++ show b
  show (RRCConnectionRequest m) = "RRCConnectionRequest "++show m
  show (RRCConnectionSetup m) = "RRCConnectionSetup "++show m
  show (RRCConnectionSetupComplete m) = "RRCConnectionComplete "++show m-}

{-data CriticalExtensions =
    RRCConnectionRequestIEs {
    ueIdentity :: InitialUEIdentity,
    establishmentCause :: EstablishmentCause,
    spare :: Int--BS.ByteString --size 1
    }
  |  CriticalExtensionFuture !Int --to be changed
  deriving (Eq, Show, Generic)

instance Binary CriticalExtensions

data InitialUEIdentity =
        STMSI {stmsi :: !Int }--See real implementation
     |  RandomValue BS.ByteString --size 40
     deriving (Eq, Show, Generic)
instance Binary InitialUEIdentity


data EstablishmentCause =
    Emergency
  | HighPriorityAccess
  | MtAccess
  | MoSignalling
  | MoData
  | DelayTolerantAccess
  | Spare2
  | Spare1
  deriving (Eq, Show, Generic)
instance Binary EstablishmentCause -}

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

data DedicatedInfoType =
     DedicatedInfoNAS
   | Other
     deriving (Eq, Generic, Show)

instance Binary DedicatedInfoType
   where put m = do
                  case m of
                   DedicatedInfoNAS -> do
                    putWord8 0
                   Other -> do
                    putWord8 1
         get = do id<- getWord8
                  case id of
                      0 ->do
                        return DedicatedInfoNAS
                      1 -> do
                        return Other

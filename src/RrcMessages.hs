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
import Control.Applicative
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

instance Binary RrcMessage where
  put m = do
    --Add a message type prefix
    case m of
      RaPreamble a b -> putWord8 0 >> put a >> put b
      RaResponse a b c -> putWord8 1 >> put a >> put b >> put c
      RrcConnectionRequest a b c -> putWord8 2 >> put a >> put b >> put c
      RrcConnectionSetup a b c -> putWord8 3 >> put a >> put b >> put c
      RrcConnectionSetupComplete a b -> putWord8 4 >> put a >> put b
      SecurityModeCommand a b-> putWord8 7 >> put a >> put b
      SecurityModeComplete a b -> putWord8 8 >> put a >> put b
      UeCapabilityEnquiry a b -> putWord8 9 >> put a >> put b
      UeCapabilityInformation a b -> putWord8 10 >> put a >> put b
      RrcConnectionReconfiguration a b -> putWord8 11 >> put a >> put b
      RrcConnectionReconfigurationComplete a b -> putWord8 12 >> put a >> put b
      RrcConnectionReject a b -> putWord8 14 >> put a >> put b
      RrcConnectionAccept a -> putWord8 15 >> put a
      EndOfProgramEnb -> putWord8 16

  get = do
    id<- getWord8
    case id of
      0 -> RaPreamble <$> get <*> get
      1 -> RaResponse <$> get <*> get <*> get
      2 -> RrcConnectionRequest <$> get <*> get <*> get
      3 -> RrcConnectionSetup <$> get <*> get <*> get
      4 -> RrcConnectionSetupComplete <$> get <*> get
      7 -> SecurityModeCommand <$> get <*> get
      8 -> SecurityModeComplete <$> get <*> get
      9 -> UeCapabilityEnquiry <$> get <*> get
      10 -> UeCapabilityInformation <$> get <*> get
      11 -> RrcConnectionReconfiguration <$> get <*> get
      12 -> RrcConnectionReconfigurationComplete <$> get <*> get
      14 -> RrcConnectionReject <$> get <*> get
      15 -> RrcConnectionAccept <$> get
      16 -> return (EndOfProgramEnb)

--Enumerated Data Types

data UeIdRntiType =
    RA_RNTI
  | C_RNTI
    deriving (Eq, Generic, Show)
             
instance Binary UeIdRntiType where
  put m = do
    case m of
      RA_RNTI -> putWord8 0
      C_RNTI -> putWord8 1
  get = do
    id<- getWord8
    case id of
      0 -> return RA_RNTI
      1 -> return C_RNTI 

data Rat =
    E_UTRA
  | UTRA
  | GERAN_CS
  | GERAN_PS
  | CDMA2000
    deriving (Eq, Generic, Show)

instance Binary Rat where
  put m = do
    case m of
      E_UTRA -> putWord8 0
      UTRA -> putWord8 1
      GERAN_CS -> putWord8 2
      GERAN_PS -> putWord8 3
      CDMA2000 -> putWord8 4
  get = do
    id<- getWord8
    case id of
      0 -> return E_UTRA
      1 -> return UTRA
      2 -> return GERAN_CS
      3 -> return GERAN_PS
      4 -> return CDMA2000 

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
    

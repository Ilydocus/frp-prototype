{-# LANGUAGE DeriveGeneric #-}
module S1Messages
       ( S1ApMessage (..)
       , encode
       , decode
       , EpsAttach (..)
       , S1MessageType (..)
       , LastMessage (..)  
       ) where

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get 
import GHC.Generics
import qualified Data.ByteString as BS
import Identifiers
import Control.Applicative
import Network.Socket
import RrcMessages

data S1ApMessage =
    S1ApInitialUeMessage {enb_Ue_S1Ap_Id :: !Int
                         ,epsAttachType :: EpsAttach
                         ,identity :: !Imsi}
  | S1ApInitialContextSetupRequest {mme_Ue_S1Ap_Id :: !Int
                                   ,enb_Ue_S1Ap_Id :: !Int
                                   ,securityKey :: !Int
                                   ,epsBearerId :: !String}
  | S1ApInitialContextSetupResponse {enb_Ue_S1Ap_Id :: !Int
                                    ,eRabId :: !String}
  | EndOfProgramMme   
  deriving (Eq, Generic, Show)

instance Binary S1ApMessage where
  put m = do
    --Add a message type prefix
    case m of
      S1ApInitialUeMessage a b c -> putWord8 5 >> put a >> put b >> put c
      S1ApInitialContextSetupRequest a b c d -> putWord8 6 >> put a >> put b >> put c >> put d
      S1ApInitialContextSetupResponse a b -> putWord8 13 >> put a >> put b 
      EndOfProgramMme -> putWord8 17

  get = do
    id<- getWord8
    case id of
      5 -> S1ApInitialUeMessage <$> get <*> get <*> get
      6 -> S1ApInitialContextSetupRequest <$> get <*> get <*> get <*> get
      13 -> S1ApInitialContextSetupResponse <$> get <*> get
      17 -> return (EndOfProgramMme)
                      

--Enumerated Data Types

data EpsAttach =
      EpsAttach
    | EpsOther
    deriving (Eq, Generic, Show)
             
instance Binary EpsAttach where
  put m = do
    case m of
      EpsAttach -> putWord8 0
      EpsOther -> putWord8 1
  get = do
    id<- getWord8
    case id of
      0 -> return EpsAttach
      1 -> return EpsOther
                        
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
  | ReconfigurationCompleted (S1ApMessage,Socket,RrcMessage, Socket)
    deriving (Eq,Show)

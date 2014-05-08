{-# LANGUAGE DeriveGeneric #-}
module UeContextStates
       ( UeContext_eNB (..)
       , UeContext_mme (..)
       , UeContext_ue (..)
       , RRCState (..)
       , defaultUeState  
       ) where

import RrcMessages
import Identifiers

{-----------------------------------------------------
     Context Definitions
------------------------------------------------------}

data UeContext_eNB = UeContext_eNB{
  rrcState :: RRCState,
  c_rnti :: !Int,
  imsi :: !IMSI,
  srbIdentity :: !String,
  eNBUES1APid :: !Int,
  ratCapabilities :: [(RAT,Bool)],
  securityKey :: !Int,
  epsBearerId :: !String  
  }

instance Show UeContext_eNB
  where
    show m = "UeContext_eNB {rrc state:"++ show (rrcState m) ++ " c-rnti:" ++ show (c_rnti m)++ " imsi:" ++ show (imsi m)++ "srb Identity:"++show (UeContextStates.srbIdentity m)++"enb S1 Identity:"++show (eNBUES1APid m)++ "}"
--affichage incomplet
data RRCState =
    RRC_Idle
  | RRC_Connected
    deriving Show

data UeContext_mme = UeContext_mme{
  mmeUES1APid :: !Int,
  securityKey_mme :: !Int
  }

instance Show UeContext_mme
  where
    show m = "UeContext_mme {MME S1AP UE ID: "++ show (mmeUES1APid m) ++ " Security Key: " ++ show (securityKey_mme m)++ "}"

data UeContext_ue = UeContext_ue{
  imsi_ue :: !IMSI,
  securityKey_ue :: !Int,
  srbId :: !String
  }

instance Show UeContext_ue
  where
    show m = "UeContext_ue {IMSI: "++ show (imsi_ue m) ++ " Security Key: " ++ show (securityKey_ue m) ++ " SRB ID: " ++ show (srbId m)++ "}"

{-----------------------------------------------------
     Default Contents
------------------------------------------------------}
defaultUeState :: Int -> UeContext_ue
defaultUeState seed = UeContext_ue{
  imsi_ue = genIMSI seed  (seed+2),
  securityKey_ue = seed*18,
  srbId = "0"
  }

{-----------------------------------------------------
     Modifiers
------------------------------------------------------}

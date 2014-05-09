{-# LANGUAGE DeriveGeneric #-}
module UeContextStates
       ( UeContext_eNB (..)
       , UeContext_mme (..)
       , UeContext_ue (..)
       , RRCState (..)
       , initialUeState
       , initialUeContext_enb
       , defaultUeContext_enb  
       , initialUeContext_mme  
       , defaultUeContext_mme  
       , addSrb_ue
       , addSrb_enb
       , addImsi_enb
       , changeStateAddEnbId_enb
       , addSecurityKeyEpsId_enb  
       , changeStateToIdle_enb
       , addRat_enb  
       ) where

import Network.Socket
import qualified RrcMessages as Rrc
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
  ratCapabilities :: [(Rrc.RAT,Bool)],
  securityKey :: !Int,
  epsBearerId :: !String  
  }

instance Show UeContext_eNB
  where
    show m = "UeContext_eNB {rrc state:"++ show (rrcState m) ++ " c-rnti:" ++ show (c_rnti m)++ " imsi:" ++ show (imsi m)++ "srb Identity:"++show (srbIdentity m)++"enb S1 Identity:"++show (eNBUES1APid m)++ "}"
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
initialUeState :: Int -> UeContext_ue
initialUeState seed = UeContext_ue{
  imsi_ue = genIMSI seed  (seed+2),
  securityKey_ue = seed*18,
  srbId = "0"
  }

initialUeContext_enb :: Int -> UeContext_eNB
initialUeContext_enb crnti = UeContext_eNB {
  rrcState = RRC_Idle,
  c_rnti = crnti,
  imsi = IMSI "0" "0" "0",
  srbIdentity = "0",
  eNBUES1APid = 0,
  ratCapabilities =[(Rrc.E_UTRA,False)],
  securityKey = 0,
  epsBearerId = "0" }

defaultUeContext_enb :: UeContext_eNB
defaultUeContext_enb = UeContext_eNB {rrcState = RRC_Idle,
  c_rnti = -1,
  imsi = IMSI "0" "0" "0",
  srbIdentity = "0",
  eNBUES1APid = 0,
  ratCapabilities =[(Rrc.E_UTRA,False)],
  securityKey = 0,
  epsBearerId = "0" }

initialUeContext_mme :: Int -> Int -> UeContext_mme
initialUeContext_mme mmeId securityKey= UeContext_mme{
  mmeUES1APid = mmeId,
  securityKey_mme = securityKey
  }

defaultUeContext_mme :: UeContext_mme
defaultUeContext_mme = UeContext_mme{
  mmeUES1APid = -1,
  securityKey_mme = -1
  }

{-----------------------------------------------------
     Modifiers
------------------------------------------------------}
addSrb_ue :: UeContext_ue -> (Rrc.RrcMessage,Socket) -> UeContext_ue
addSrb_ue oldState (message, _)= UeContext_ue{
  imsi_ue = imsi_ue oldState,
  securityKey_ue = securityKey_ue oldState,
  srbId = Rrc.srbIdentity message
  }

addSrb_enb :: String -> UeContext_eNB -> UeContext_eNB
addSrb_enb srb oldContent =  UeContext_eNB {
  rrcState = rrcState oldContent,
  c_rnti = c_rnti oldContent,
  imsi = imsi oldContent,
  srbIdentity = srb,
  eNBUES1APid =eNBUES1APid oldContent ,
  ratCapabilities =ratCapabilities oldContent,
  securityKey = securityKey oldContent,
  epsBearerId = epsBearerId oldContent
  }

addImsi_enb :: IMSI -> UeContext_eNB -> UeContext_eNB
addImsi_enb imsi oldContent = UeContext_eNB {
  rrcState = rrcState oldContent,
  c_rnti = c_rnti oldContent,
  imsi = imsi,
  srbIdentity = srbIdentity oldContent,
  eNBUES1APid =eNBUES1APid oldContent ,
  ratCapabilities =ratCapabilities oldContent,
  securityKey = securityKey oldContent,
  epsBearerId = epsBearerId oldContent
  }

changeStateAddEnbId_enb :: Int -> UeContext_eNB -> UeContext_eNB
changeStateAddEnbId_enb enbId oldContent =  UeContext_eNB {
  rrcState =RRC_Connected,
  c_rnti = c_rnti oldContent,
  imsi = imsi oldContent,
  srbIdentity = srbIdentity oldContent,
  eNBUES1APid =enbId,
  ratCapabilities =ratCapabilities oldContent,
  securityKey = securityKey oldContent,
  epsBearerId = epsBearerId oldContent
  }

addSecurityKeyEpsId_enb :: Int -> String -> UeContext_eNB -> UeContext_eNB
addSecurityKeyEpsId_enb key epsId oldContent =  UeContext_eNB{
  rrcState =rrcState oldContent,
  c_rnti = c_rnti oldContent,
  imsi = imsi oldContent,
  srbIdentity = srbIdentity oldContent,
  eNBUES1APid =eNBUES1APid oldContent,
  ratCapabilities =ratCapabilities oldContent,
  securityKey = key,
  epsBearerId = epsId
  }

changeStateToIdle_enb :: UeContext_eNB -> UeContext_eNB
changeStateToIdle_enb oldContent =  UeContext_eNB {
  rrcState =RRC_Idle,
  c_rnti = c_rnti oldContent,
  imsi = imsi oldContent,
  srbIdentity = srbIdentity oldContent,
  eNBUES1APid =eNBUES1APid oldContent,
  ratCapabilities =ratCapabilities oldContent,
  securityKey = securityKey oldContent,
  epsBearerId = epsBearerId oldContent
  }

addRat_enb :: [(Rrc.RAT,Bool)] -> UeContext_eNB -> UeContext_eNB
addRat_enb ratList oldContent =  UeContext_eNB {
  rrcState =rrcState oldContent,
  c_rnti = c_rnti oldContent,
  imsi = imsi oldContent,
  srbIdentity = srbIdentity oldContent,
  eNBUES1APid =eNBUES1APid oldContent,
  ratCapabilities =ratList,
  securityKey = securityKey oldContent,
  epsBearerId = epsBearerId oldContent
  }


        

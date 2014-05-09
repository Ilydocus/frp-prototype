{-# LANGUAGE DeriveGeneric #-}
module UeContextStates
       ( UeContext_enb (..)
       , UeContext_mme (..)
       , UeContext_ue (..)
       , RrcState (..)
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

data UeContext_enb = UeContext_enb{
  rrcState :: RrcState,
  c_rnti :: !Int,
  imsi :: !Imsi,
  srbIdentity :: !String,
  enbUeS1ApId :: !Int,
  ratCapabilities :: [(Rrc.Rat,Bool)],
  securityKey :: !Int,
  epsBearerId :: !String  
  }

instance Show UeContext_enb
  where
    show m = "UeContext_eNB {RRC state: "++ show (rrcState m) ++ " C-RNTI: " ++ show (c_rnti m)++ " IMSI: " ++ show (imsi m)++ " SRB Identity: "++show (srbIdentity m)++" eNB S1 Identity:"++show (enbUeS1ApId m)++ " RAT Capabilities: "++show (ratCapabilities m)++" Security Key: "++show (securityKey m)++" EPS Bearer Id: "++show (epsBearerId m)++"}"

data RrcState =
    RRC_Idle
  | RRC_Connected
    deriving Show

data UeContext_mme = UeContext_mme{
  mmeUeS1ApId :: !Int,
  securityKey_mme :: !Int
  }

instance Show UeContext_mme
  where
    show m = "UeContext_mme {MME S1AP UE ID: "++ show (mmeUeS1ApId m) ++ " Security Key: " ++ show (securityKey_mme m)++ "}"

data UeContext_ue = UeContext_ue{
  imsi_ue :: !Imsi,
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
  imsi_ue = genImsi seed  (seed+2),
  securityKey_ue = seed*18,
  srbId = "0"
  }

initialUeContext_enb :: Int -> UeContext_enb
initialUeContext_enb crnti = UeContext_enb {
  rrcState = RRC_Idle,
  c_rnti = crnti,
  imsi = Imsi "0" "0" "0",
  srbIdentity = "0",
  enbUeS1ApId = 0,
  ratCapabilities =[(Rrc.E_UTRA,False)],
  securityKey = 0,
  epsBearerId = "0" }

defaultUeContext_enb :: UeContext_enb
defaultUeContext_enb = UeContext_enb {rrcState = RRC_Idle,
  c_rnti = -1,
  imsi = Imsi "0" "0" "0",
  srbIdentity = "0",
  enbUeS1ApId = 0,
  ratCapabilities =[(Rrc.E_UTRA,False)],
  securityKey = 0,
  epsBearerId = "0" }

initialUeContext_mme :: Int -> Int -> UeContext_mme
initialUeContext_mme mmeId securityKey= UeContext_mme{
  mmeUeS1ApId = mmeId,
  securityKey_mme = securityKey
  }

defaultUeContext_mme :: UeContext_mme
defaultUeContext_mme = UeContext_mme{
  mmeUeS1ApId = -1,
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

addSrb_enb :: String -> UeContext_enb -> UeContext_enb
addSrb_enb srb oldContent =  UeContext_enb {
  rrcState = rrcState oldContent,
  c_rnti = c_rnti oldContent,
  imsi = imsi oldContent,
  srbIdentity = srb,
  enbUeS1ApId =enbUeS1ApId oldContent ,
  ratCapabilities =ratCapabilities oldContent,
  securityKey = securityKey oldContent,
  epsBearerId = epsBearerId oldContent
  }

addImsi_enb :: Imsi -> UeContext_enb -> UeContext_enb
addImsi_enb imsi oldContent = UeContext_enb {
  rrcState = rrcState oldContent,
  c_rnti = c_rnti oldContent,
  imsi = imsi,
  srbIdentity = srbIdentity oldContent,
  enbUeS1ApId =enbUeS1ApId oldContent ,
  ratCapabilities =ratCapabilities oldContent,
  securityKey = securityKey oldContent,
  epsBearerId = epsBearerId oldContent
  }

changeStateAddEnbId_enb :: Int -> UeContext_enb -> UeContext_enb
changeStateAddEnbId_enb enbId oldContent =  UeContext_enb {
  rrcState =RRC_Connected,
  c_rnti = c_rnti oldContent,
  imsi = imsi oldContent,
  srbIdentity = srbIdentity oldContent,
  enbUeS1ApId =enbId,
  ratCapabilities =ratCapabilities oldContent,
  securityKey = securityKey oldContent,
  epsBearerId = epsBearerId oldContent
  }

addSecurityKeyEpsId_enb :: Int -> String -> UeContext_enb -> UeContext_enb
addSecurityKeyEpsId_enb key epsId oldContent =  UeContext_enb{
  rrcState =rrcState oldContent,
  c_rnti = c_rnti oldContent,
  imsi = imsi oldContent,
  srbIdentity = srbIdentity oldContent,
  enbUeS1ApId =enbUeS1ApId oldContent,
  ratCapabilities =ratCapabilities oldContent,
  securityKey = key,
  epsBearerId = epsId
  }

changeStateToIdle_enb :: UeContext_enb -> UeContext_enb
changeStateToIdle_enb oldContent =  UeContext_enb {
  rrcState =RRC_Idle,
  c_rnti = c_rnti oldContent,
  imsi = imsi oldContent,
  srbIdentity = srbIdentity oldContent,
  enbUeS1ApId =enbUeS1ApId oldContent,
  ratCapabilities =ratCapabilities oldContent,
  securityKey = securityKey oldContent,
  epsBearerId = epsBearerId oldContent
  }

addRat_enb :: [(Rrc.Rat,Bool)] -> UeContext_enb -> UeContext_enb
addRat_enb ratList oldContent =  UeContext_enb {
  rrcState =rrcState oldContent,
  c_rnti = c_rnti oldContent,
  imsi = imsi oldContent,
  srbIdentity = srbIdentity oldContent,
  enbUeS1ApId =enbUeS1ApId oldContent,
  ratCapabilities =ratList,
  securityKey = securityKey oldContent,
  epsBearerId = epsBearerId oldContent
  }


        

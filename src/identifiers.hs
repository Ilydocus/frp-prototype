module Identifiers (
             IMSI (..)
                   )
       where

import System.Random
import Control.Monad.Random
import Control.Monad.IO.Class ()
import Control.Monad
import System.IO

-- IMSI

genIMSI :: IMSI
genIMSI = --all the imsi are swedish ones
  IMSI {mcc = "260", mnc = genRandId 2 478 , msin = genRandId 10 856}

data IMSI = IMSI {
  mcc :: !String,
  mnc :: !String,
  msin :: !String
  }

instance Show IMSI
         where
           show m = (show (mcc m)) ++ (show (mnc m)) ++ (show (msin m))

-- Random id string
           
genRandId :: Int -> Int -> String
genRandId size seed =
  take size (randomRs ('0','9') (mkStdGen seed))
   {-| size <= 0 = "" 
   | otherwise = (take size (randomRs ('0','9') (mkStdGen seed))) ++ (genRandId (size -1) (seed))-}

 

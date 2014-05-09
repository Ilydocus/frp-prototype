module Identifiers (
             IMSI (..),
             genIMSI,
             genRandId
                   )
       where

import System.Random
import Control.Monad.IO.Class ()
import Control.Monad
import System.IO
import Data.Binary

-- IMSI
data IMSI = IMSI {
  mcc :: !String,
  mnc :: !String,
  msin :: !String
  }
            deriving (Eq)

instance Show IMSI
         where
           show m = show ((mcc m)++"-"++(mnc m)++"-"++(msin m))

instance Binary IMSI
         where
           put m = do
             put (mcc m)
             put (mnc m)
             put (msin m)
           get = do
                 mccTemp <- get
                 mncTemp <- get
                 msinTemp <- get
                 return (IMSI mccTemp mncTemp msinTemp)

genIMSI :: Int -> Int -> IMSI
genIMSI seed1 seed2= --all the imsi are swedish ones
  IMSI {mcc = "260", mnc = genRandId 2 seed1 , msin = genRandId 10 seed2}

-- Random id string
genRandId :: Int -> Int -> String
genRandId size seed =
  take size (randomRs ('0','9') (mkStdGen seed))


 

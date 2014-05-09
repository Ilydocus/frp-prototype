module Identifiers (
             Imsi (..),
             genImsi,
             genRandId
                   )
       where

import System.Random
import Control.Monad.IO.Class ()
import Data.Binary

-- IMSI
data Imsi = Imsi {
  mcc :: !String,
  mnc :: !String,
  msin :: !String
  }
            deriving (Eq)

instance Show Imsi
         where
           show m = show ((mcc m)++"-"++(mnc m)++"-"++(msin m))

instance Binary Imsi
         where
           put m = do
             put (mcc m)
             put (mnc m)
             put (msin m)
           get = do
                 mccTemp <- get
                 mncTemp <- get
                 msinTemp <- get
                 return (Imsi mccTemp mncTemp msinTemp)

genImsi :: Int -> Int -> Imsi
genImsi seed1 seed2= --all the imsi are swedish ones
  Imsi {mcc = "260", mnc = genRandId 2 seed1 , msin = genRandId 10 seed2}

-- Random id string
genRandId :: Int -> Int -> String
genRandId size seed =
  take size (randomRs ('0','9') (mkStdGen seed))


 

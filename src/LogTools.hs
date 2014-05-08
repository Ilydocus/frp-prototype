module LogTools (writeToLog
                ,finalLog_ue)
       where

import Data.Time
import System.IO
import RrcMessages
import UeContextStates
import Network.Socket

writeToLog :: Handle -> String -> IO ()
writeToLog handle message = do
  time <- getCurrentTime
  hPutStrLn handle (show time ++ " : " ++ message)

finalLog_ue :: Handle -> UeContext_ue -> (RrcMessage, Socket) -> IO()
finalLog_ue handle state (message, _) = do
  writeToLog handle ("UE Context at the end : "++ show state)

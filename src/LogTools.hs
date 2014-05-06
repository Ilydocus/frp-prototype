module LogTools (writeToLog)
       where

import Data.Time

writeToLog :: Handle -> String -> IO ()
writeToLog handle message = do
  time <- getCurrentTime
  hPutStrLn handle (show time ++ " : " ++ message)

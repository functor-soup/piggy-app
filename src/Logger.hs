module Logger
  ( debugMsg
  , errorMsg
  , infoMsg
  , verboseMsg
  ) where

putMsg :: String -> String -> IO ()
putMsg x y = putStrLn $ "[" ++ x ++ "] " ++ y

debugMsg :: String -> IO ()
debugMsg = putMsg "DEBUG"

errorMsg :: String -> IO ()
errorMsg = putMsg "ERROR"

infoMsg :: String -> IO ()
infoMsg = putMsg "INFO"

verboseMsg :: String -> IO ()
verboseMsg = putMsg "VERBOSE"

module Logger
  ( debugMsg
  , errorMsg
  , infoMsg
  , verboseMsg
  ) where

type GenMsgType = String -> String

putMsg :: String -> String -> String
putMsg x y = "[" ++ x ++ "] " ++ y ++ "\n"

debugMsg :: GenMsgType
debugMsg = putMsg "DEBUG"

errorMsg :: GenMsgType
errorMsg = putMsg "ERROR"

infoMsg :: GenMsgType
infoMsg = putMsg "INFO"

verboseMsg :: GenMsgType
verboseMsg = putMsg "VERBOSE"

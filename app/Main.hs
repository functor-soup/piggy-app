module Main where

import Control.Exception.Base (displayException)
import Control.Monad.Trans.Either
import Lib
import Logger
import Utilz
import Data.Function

configFile :: String
configFile = "config.yaml"

main :: IO ()
main =
  (runEitherT $ getFile configFile & bimapEitherT displayException id >>= getUrl) >>= print

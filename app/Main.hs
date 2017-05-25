module Main where

import Control.Exception.Base (displayException)
import Control.Monad.Trans.Either
import Data.Bifunctor
import Lib
import Logger
import Utilz

configFile :: String
configFile = "config.yaml"

main :: IO ()
main =
  print (runEitherT $ getFile configFile >>= bimapEitherT displayException id >>= getUrl)

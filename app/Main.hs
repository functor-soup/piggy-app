module Main where

import Client
import Control.Exception.Base (displayException)
import Control.Monad.Trans.Either
import Data.Function
import Lib
import Logger
import Types
import Utilz
import Web.Scotty

configFile :: String
configFile = "config.yaml"

getAllRoute :: String -> EitherT String (WriterT String ActionM) ()
getAllRoute url = do
  lift . tell $ "User sent a GET request to /"
  (getTicker >=> processGetTickerResponse >=> (lift . lift . raw)) $ url

getConversionRoute :: String -> EitherT String (WriterT String ActionM) ()
getConversionRoute url = do
  x <- lift . lift . body
  let decoded_ = (eitherdecode x) :: Either String PostConversion
  case decoded_ of
    Left x -> (lift . tell . infoMsg $ x) >> left x
    Right x ->
      (lift . tell . infoMsg $ "Client sent a POST req to /" ++ show x) >>
      (getConversion url (country x) (numbah x)) >>=
      (lift . lift . raw)

-- handle POST route
main :: IO ()
main = (runEitherT $ getFile configFile & bimapEitherT displayException id >>= getUrl) >>= print

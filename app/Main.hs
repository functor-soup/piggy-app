{-# LANGUAGE OverloadedStrings #-}

module Main where

import Client
import Control.Exception.Base (displayException)
import Control.Monad.Trans.Either
import Control.Monad.Writer.Lazy
import Data.Aeson
import Data.Function
import Data.String
import Lib
import Logger
import Types
import Utilz
import Web.Scotty

configFile :: String
configFile = "config.yaml"

getAllRoute :: String -> ActionM ()
getAllRoute url = do
  (v, logs) <-
    liftIO $
    runWriterT . runEitherT $
    (lift . tell . infoMsg $ "User sent a GET request to /") >>
    (getTicker >=> processGetTickerResponse $ url)
  liftIO . putStrLn $ logs
  case v of
    Left a -> liftIO . putStrLn . errorMsg $ a
    Right b -> raw b

getConversionRoute :: String -> ActionM ()
getConversionRoute url = do
  x <- body
  let decoded_ = (eitherDecode x) :: Either String PostConversion
  (v, logs) <-
    liftIO $
    runWriterT . runEitherT $
    (case decoded_ of
       Left x -> (lift . tell . infoMsg $ x) >> left x
       Right x ->
         (lift . tell . infoMsg $ "Client sent a POST req to /" ++ show x) >>
         (getConversion url (country x) (numbah x)))
  liftIO . putStrLn $ logs
  case v of
    Left a -> liftIO . putStrLn . errorMsg $ a
    Right b -> raw b

routes :: String -> ScottyM ()
routes baseUrl = (post "/" $ getConversionRoute baseUrl) >> (get "/" $ getAllRoute baseUrl)

-- handle POST route
main :: IO ()
main =
  (runEitherT $ getFile configFile & bimapEitherT displayException id >>= getUrl) >>=
  (\x ->
     case x of
       Left a -> putStrLn a
       Right b -> scotty 8000 $ routes b)

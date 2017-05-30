{-# LANGUAGE OverloadedStrings #-}

module Client
  ( getTicker
  , getConversion
  , processGetTickerResponse
  ) where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.Trans.Either
import Control.Monad.Writer.Lazy
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Function
import Logger
import Network.HTTP.Client (HttpException)
import Network.Wreq
import Types

-- TODO: should make the eitherT stuff a type synonym
--  is the use of 'left' necessary?
getCall :: String -> EitherT HttpException (WriterT String IO) (Response B.ByteString)
getCall url = do
  lift . tell $ (infoMsg $ "Http client call to " ++ url)
  catch (liftIO $ get url) (\x -> tell (errorMsg $ displayException x) >> left x)

getTicker :: String -> EitherT String (WriterT String IO) (Response B.ByteString)
getTicker baseUrl = (getCall $ baseUrl ++ "/ticker") & bimapEitherT displayException id

getConversion :: String -> Country -> Float -> EitherT String (WriterT String IO) B.ByteString
getConversion url country numbah =
  let url_ = url ++ "tobtc?currency=" ++ (show country) ++ "&value=" ++ (show numbah)
  in (getCall url_) & bimapEitherT displayException (\x -> x ^. responseBody)

-- log custom version of decoded Json
processGetTickerResponse :: (Response B.ByteString)
                         -> EitherT String (WriterT String IO) B.ByteString
processGetTickerResponse resp = do
  let decoded_ = (resp ^. responseBody & eitherDecode) :: Either String GetAll
  case decoded_ of
    Right a -> (lift . tell $ infoMsg "Server sent back " ++ show a) >> right (resp ^. responseBody)
    Left b -> (lift . tell $ errorMsg b) >> left b

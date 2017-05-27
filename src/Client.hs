{-# LANGUAGE OverloadedStrings #-}

module Client
  ( getCall
  , getConversion
  ) where

import Control.Monad.Catch
import Control.Monad.Trans.Either
import Control.Monad.Writer.Lazy
import qualified Data.ByteString.Lazy as B
import Logger
import Network.HTTP.Client
import Network.Wreq
import Types

-- TODO: should make the eitherT stuff a type synonym
getCall :: String -> EitherT HttpException (WriterT String IO) (Response B.ByteString)
getCall url = do
  lift . tell $ (infoMsg $ "Http client call to " ++ url)
  catch (liftIO $ get url) (\x -> tell (errorMsg $ displayException x) >> left x)

getTicker :: String -> EitherT HttpException (WriterT String IO) (Response B.ByteString)
getTicker baseUrl = getCall $ baseUrl ++ "/ticker"

getConversion :: String
              -> Country
              -> Float
              -> EitherT HttpException (WriterT String IO) (Response B.ByteString)
getConversion url country numbah =
  let url_ = url ++ "tobtc?currency=" ++ (show country) ++ "&value=" ++ (show numbah)
  in getTicker url_

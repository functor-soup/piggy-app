{-# LANGUAGE DeriveGeneric #-}

module Utilz
  ( ConfigUrl(..)
  , getFile
  , getUrl
  ) where

import Control.Exception.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import qualified Data.ByteString as B
import Data.Yaml
import GHC.Generics
import System.Directory

data ConfigUrl = ConfigUrl
  { url :: String
  } deriving (Generic)

instance FromJSON ConfigUrl

-- my wrapper for getFileContents
getFile :: String -> EitherT IOException IO B.ByteString
getFile fileName = Control.Monad.Catch.catch (liftIO $ B.readFile fileName) left

getUrl :: B.ByteString -> EitherT String IO String
getUrl = hoistEither . decodeEither

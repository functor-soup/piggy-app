module Main where

import Control.Exception.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Lib
import System.Directory

data MyException =
  FileNotFound
  deriving (Show)

instance Exception MyException where
  displayException _ = "File wasn't found!"

configFile :: String
configFile = "config.yaml"

-- my wrapper for getFileContents
getFile
  :: (Exception k)
  => String -> EitherT k IO String
getFile fileName =
  liftIO (doesFileExist fileName) >>=
  (\x ->
     case x of
       True -> right fileName
       False -> throwM FileNotFound) >>=
  (\x -> Control.Monad.Catch.catch (liftIO $ readFile x) (\y -> left $ y))

main :: IO ()
main = putStrLn "hello world"

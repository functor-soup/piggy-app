{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Country
  , GetAllMap
  , GetAll
  ) where

import Data.Aeson
import Data.HashMap.Lazy
import GHC.Generics
import Prelude hiding (last)

data Country
  = USD
  | ISK
  | HKD
  | TWD
  | CHF
  | EUR
  | DKK
  | CLP
  | CAD
  | INR
  | CNY
  | THB
  | AUD
  | SGD
  | KRW
  | JPY
  | PLN
  | GBP
  | SEK
  | NZD
  | BRL
  | RUB
  deriving (Show, Generic)

data GetAllMap = GetAllMap
  { numbahm :: Float
  , last :: Float
  , buy :: Float
  , sell :: Float
  , symbol :: String
  }

instance Show GetAllMap where
  show (GetAllMap a b c d e) =
    " {{ 15m  |**| " ++
    (show a) ++
    " :: last |**| " ++
    (show b) ++
    " :: buy |**| " ++
    (show c) ++ " :: sell |**| " ++ (show d) ++ " :: symbol |**| " ++ (show e) ++ " }}"

instance FromJSON GetAllMap where
  parseJSON =
    withObject "GetAllMap" $ \v ->
      GetAllMap <$> v .: "15m" <*> v .: "last" <*> v .: "buy" <*> v .: "sell" <*> v .: "symbol"

data GetAll =
  GetAll (HashMap Country GetAllMap)
  deriving (Show)

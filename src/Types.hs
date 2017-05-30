{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Country
  , GetAllMap
  , GetAll
  ) where

import Data.Aeson
import Data.Map.Lazy (Map)
import qualified Data.Text as Text
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
  deriving (Eq, Show, Generic, Ord, Read)

instance FromJSON Country

instance FromJSONKey Country where
  fromJSONKey = FromJSONKeyText (read . Text.unpack)

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

type GetAll = Map Country GetAllMap

data PostConversion = PostConversion
  { country :: Country
  , numbah :: Float
  } deriving (Generic, Eq)

instance FromJSON PostConversion

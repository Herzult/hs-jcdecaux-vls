{-# LANGUAGE OverloadedStrings #-}

module JCDecaux.Vls.Api
        ( contracts
        , stations
        , contractStations
        , contractStation
        , ApiKey(..)
        )
    where


import JCDecaux.Vls.Types

import Network.HTTP.Conduit (simpleHttp)

import Control.Monad
import Control.Monad.IO.Class

import Data.Text (Text, pack, unpack, intercalate)
import Data.String (IsString, fromString)

import Data.Aeson (FromJSON, eitherDecode)
import Data.Monoid ((<>))

type Error = String

type Path = [Text]

type QueryString = [(Text, Text)]

newtype ApiKey = ApiKey Text

instance IsString ApiKey where
   fromString = ApiKey . pack

type Result a = Either Error a

contracts :: MonadIO m => ApiKey -> m (Result [Contract])
contracts key = get key ["contracts"] []

stations :: MonadIO m => ApiKey -> m (Result [Station])
stations key = get key ["stations"] []

contractStations :: MonadIO m
                 => ApiKey
                 -> Text -- ^ contract name
                 -> m (Result [Station])
contractStations key cn = get key ["stations"] [("contract", cn)]

contractStation :: MonadIO m
                => ApiKey
                -> Text -- ^ contract name
                -> Int  -- ^ station number
                -> m (Result Station)
contractStation key cn sn = get key ["stations", pack (show sn)] [("contract", cn)]

get :: (MonadIO m, FromJSON a) => ApiKey -> Path -> QueryString -> m (Result a)
get (ApiKey k) p q =
    liftM eitherDecode (simpleHttp $ unpack url)
  where
    url   = "https://api.jcdecaux.com/vls/v1/" <> intercalate "/" p <> "?" <> query
    query = intercalate "&" $ map (\(n, v) -> n <> "=" <> v) (("apiKey", k):q)

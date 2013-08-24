{-# LANGUAGE OverloadedStrings #-}

module JCDecaux.Vls.Types
        ( Position(..)
        , StationStatus(..)
        , Station(..)
        , Contract(..)
        )
    where

import Data.Aeson
import Data.Text (Text)
import Control.Applicative
import Control.Monad

-- WGS 84 position
data Position = Position {
    positionLatitude  :: Double
  , positionLongitude :: Double
} deriving (Show, Eq)

instance FromJSON Position where
    parseJSON (Object v) = Position <$> v .: "lat" <*> v .: "lng"
    parseJSON _ = mzero

instance ToJSON Position where
    toJSON (Position lat lng) = object ["lat" .= toJSON lat, "lng" .= toJSON lng]

type City = Text

data Contract = Contract {
    contractName           :: Text
  , contractCommercialName :: Text
  , contractCities         :: [City]
  , contractCountryCode    :: Text
} deriving (Show, Eq)

instance FromJSON Contract where
    parseJSON (Object v) = Contract
                       <$> v .: "name"
                       <*> v .: "commercial_name"
                       <*> v .: "cities"
                       <*> v .: "country_code"
    parseJSON _ = mzero

instance ToJSON Contract where
    toJSON c = object ["name"            .= contractName c
                      ,"commercial_name" .= contractCommercialName c
                      ,"cities"          .= contractCities c
                      ,"country_code"    .= contractCountryCode c
                      ]

data StationStatus = Open | Closed deriving (Show, Eq)

instance FromJSON StationStatus where
    parseJSON (String "OPEN")   = pure Open
    parseJSON (String "CLOSED") = pure Closed
    parseJSON _ = mzero

instance ToJSON StationStatus where
    toJSON Open   = toJSON ("OPEN" :: Text)
    toJSON Closed = toJSON ("CLOSED" :: Text)

data Station = Station {
    stationNumber              :: Int
  , stationName                :: Text
  , stationAddress             :: Text
  , stationPosition            :: Position
  , stationBanking             :: Bool
  , stationBonus               :: Bool
  , stationStatus              :: StationStatus
  , stationBikeStands          :: Int
  , stationAvailableBikeStands :: Int
  , stationAvailableBikes      :: Int
  , stationLastUpdate          :: Integer
} deriving (Show, Eq)

instance FromJSON Station where
    parseJSON (Object v) = Station
                       <$> v .: "number"
                       <*> v .: "name"
                       <*> v .: "address"
                       <*> v .: "position"
                       <*> v .: "banking"
                       <*> v .: "bonus"
                       <*> v .: "status"
                       <*> v .: "bike_stands"
                       <*> v .: "available_bike_stands"
                       <*> v .: "available_bikes"
                       <*> v .: "last_update"
    parseJSON _ = mzero

instance ToJSON Station where
    toJSON s = object ["number"                 .= stationNumber s
                      ,"name"                   .= stationName s
                      ,"address"                .= stationAddress s
                      ,"position"               .= stationPosition s
                      ,"banking"                .= stationBanking s
                      ,"bonus"                  .= stationBonus s
                      ,"status"                 .= stationStatus s
                      ,"bike_stands"            .= stationBikeStands s
                      ,"available_bike_stands"  .= stationAvailableBikeStands s
                      ,"available_bikes"        .= stationAvailableBikes s
                      ,"last_update"            .= stationLastUpdate s
                      ]

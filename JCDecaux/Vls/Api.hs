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

import Control.Monad.IO.Class

import Data.Text (Text, pack, unpack, intercalate)
import Data.String (IsString, fromString)
import qualified Data.ByteString.Lazy.Char8 as LBS

import Data.Aeson (FromJSON, eitherDecode)
import Data.Monoid ((<>))

import qualified Control.Exception as E

data Error = HttpConnectionError E.SomeException
           | JsonError   Text
           deriving (Show)

type Path = [Text]

type QueryString = [(Text, Text)]

newtype ApiKey = ApiKey Text

instance IsString ApiKey where
   fromString = ApiKey . pack

type Result a = Either Error a

contracts :: MonadIO m => ApiKey -> m (Result [Contract])
contracts key = liftIO $ get key ["contracts"] []

stations :: MonadIO m => ApiKey -> m (Result [Station])
stations key = liftIO $ get key ["stations"] []

contractStations :: MonadIO m
                 => ApiKey
                 -> Text -- ^ contract name
                 -> m (Result [Station])
contractStations key cn = liftIO $ get key ["stations"] [("contract", cn)]

contractStation :: MonadIO m
                => ApiKey
                -> Text -- ^ contract name
                -> Int  -- ^ station number
                -> m (Result Station)
contractStation key cn sn = liftIO $ get key ["stations", pack (show sn)] [("contract", cn)]

get :: (FromJSON a) => ApiKey -> Path -> QueryString -> IO (Result a)
get (ApiKey k) p q = do
    (simpleHttp url >>= return . parse)
        `E.catches` onException

  where
    onException = [ E.Handler (\e -> E.throw (e :: E.AsyncException))
                  , E.Handler (\e -> return . Left $ HttpConnectionError (e :: E.SomeException))
                  ]

    parse b =
        case eitherDecode b of
            Left e  -> Left $ JsonError $ pack e <> " in JSON response: " <> pack (LBS.unpack b)
            Right d -> Right d

    url   = unpack $ "https://api.jcdecaux.com/vls/v1/" <> intercalate "/" p <> "?" <> query
    query = intercalate "&" $ map (\(n, v) -> n <> "=" <> v) (("apiKey", k):q)

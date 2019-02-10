{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Hoogle
    ( search )
where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

type HoogleAPI = "hoogle" :> QueryParam "mode" String :> QueryParam "hoogle" String :> Get '[JSON] Answer

data Answer = Answer
    { results :: [Result]
    , version :: String
    } deriving (Show, Generic)
instance FromJSON Answer

data Result = Result
    { location :: String
    , self     :: String
    , docs     :: String
    } deriving (Show, Generic)
instance FromJSON Result

api :: Proxy HoogleAPI
api = Proxy

hoogle :: Maybe String -> Maybe String -> ClientM Answer
hoogle = client api

search :: Text -> [Text]
search = undefined

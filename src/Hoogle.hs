{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Hoogle
    ( search )
where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics
import Servant.API
import Servant.Client

type HoogleAPI = "hoogle" :> QueryParam "mode" Json :> Capture "hoogle" String :> Get '[JSON] Answer

data Json = Json

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

hoogle :: String -> ClientM Answer
hoogle = undefined

search :: Text -> [Text]
search = undefined

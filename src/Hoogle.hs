{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Hoogle
    ( search
    , Result
    , location
    , self
    , docs
    )
where

import           Data.Text   (Text)
import qualified Data.Text as Text

import Data.Aeson (FromJSON)
import Data.Bifunctor
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client

type HoogleAPI = "hoogle" :> QueryParam "mode" String :> QueryParam "hoogle" String :> QueryParam "count" Int :> Get '[JSON] Answer

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

hoogle :: Maybe String -> Maybe String -> Maybe Int -> ClientM Answer
hoogle = client api

searchResults :: Text -> Int -> ClientM [Result]
searchResults query count = results <$> (hoogle (Just "json") (Just . Text.unpack $ query) (Just count))

search :: Text -> Int -> IO (Either Text [Result])
search query count = do
    manager <- newManager tlsManagerSettings
    res <- runClientM (searchResults query count) $ mkClientEnv manager $ BaseUrl Https "haskell.org" 443 ""
    return $ first (Text.pack . show) res

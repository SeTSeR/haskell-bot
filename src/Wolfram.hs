{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Wolfram
    ( wolfram
    )
where

import           Data.Text   (Text)
import qualified Data.Text as Text

import Codec.Picture.Types
import Data.Aeson (FromJSON)
import Data.Bifunctor
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Links
import Servant.JuicyPixels
import System.Environment

type WolframAPI = "simple" :> QueryParam "appid" String :> QueryParam "background" String :> QueryParam "foreground" String :> QueryParam "fontsize" Int :> QueryParam "layout" String :> QueryParam "i" String :> Get '[GIF] DynamicImage

token :: IO String
token = getEnv "WOLFRAM_TOKEN"

fontsize :: Int
fontsize = 14

layout :: String
layout = "divider"

background :: String
background = "white"

foreground :: String
foreground = "black"

api :: Proxy WolframAPI
api = Proxy

baseUrl :: String
baseUrl = "https://api.wolframalpha.com/v1/"

wolfram :: Text -> IO String
wolfram query = token >>= \token -> return $ baseUrl ++ (show . linkURI $ safeLink api api (Just token) (Just background) (Just foreground) (Just fontsize) (Just layout) (Just . Text.unpack $ query))

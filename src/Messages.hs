{-# LANGUAGE OverloadedStrings #-}
module Messages where

import           Data.Text   (Text)
import qualified Data.Text as Text

import Telegram.Bot.API.Types(UserId)

helpMessage :: Text
helpMessage = Text.unlines
    [ "Привет, я @myhlbot, и пока я ничего не умею."
    , "Можешь попробовать что-нибудь написать."
    ]

accessDeniedMessage :: Text
accessDeniedMessage = Text.unlines
    [ "Доступ к логам разрешён только администраторам бота." ]

broadcastMessage :: UserId -> Text
broadcastMessage userId = Text.unwords
    [ "User with id"
    , Text.pack . show $ userId
    , "has tried to broadcast.\n"
    ]

broadcastDeniedMessage :: Text
broadcastDeniedMessage = Text.unlines
    [ "Рассылка разрешена только администраторам бота." ]

pongMessage :: Text
pongMessage = Text.unlines
    [ "pong!" ]

accessMessage :: UserId -> Text
accessMessage userId = Text.unwords
    [ "User with id"
    , Text.pack . show $ userId
    , "has tried to access the logs.\n"
    ]

groupsDeniedMessage :: Text
groupsDeniedMessage = Text.unlines
    [ "Запрос логов разрешён только из лички." ]

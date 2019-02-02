{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( start
    ) where

import Control.Applicative
import           Data.Text   (Text)
import qualified Data.Text as Text

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

type Model = ()

data Action
    = NoOp
    | Echo Text
    | Show Text
    deriving (Read, Show)

echoBot :: BotApp Model Action
echoBot = BotApp 
    { botInitialModel = ()
    , botAction = flip updateToAction
    , botHandler = handleAction
    , botJobs = []
    }

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ = parseUpdate $
        Show helpMessage <$  command "start"
    <|> Show helpMessage <$  command "help"
    <|> Echo             <$> plainText
    <|> callbackQueryDataRead

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
    NoOp -> pure model
    Show message -> model <# do
        reply (toReplyMessage message)
        return NoOp
    Echo msg -> model <# do
        replyText msg
        return NoOp

helpMessage :: Text
helpMessage = Text.unlines
    [ "Привет, я @myhlbot, и пока я ничего не умею."
    , "Можешь попробовать что-нибудь написать."
    ]

run :: Token -> IO ()
run token = do
    env <- defaultTelegramClientEnv token
    startBot_ (conversationBot updateChatId echoBot) env

start :: IO ()
start = do
    token <- getEnvToken "TELEGRAM_BOT_TOKEN"
    run token

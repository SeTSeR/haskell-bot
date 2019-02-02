{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( start
    ) where

import           Data.Text   (Text)
import qualified Data.Text as Text

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser (updateMessageText)

type Model = ()

data Action = NoOp | Echo Text

echoBot :: BotApp Model Action
echoBot = BotApp 
    { botInitialModel = ()
    , botAction = updateToAction
    , botHandler = handleAction
    , botJobs = []
    }

updateToAction :: Update -> Model -> Maybe Action
updateToAction update _ = 
    case updateMessageText update of
        Just text -> Just (Echo text)
        Nothing -> Nothing

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
    NoOp -> pure model
    Echo msg -> model <# do
        replyText msg
        return NoOp

run :: Token -> IO ()
run token = do
    env <- defaultTelegramClientEnv token
    startBot_ (conversationBot updateChatId echoBot) env

start :: IO ()
start = do
    token <- getEnvToken "TELEGRAM_BOT_TOKEN"
    run token

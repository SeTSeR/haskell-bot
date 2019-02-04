{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( start
    ) where

import Control.Applicative
import Control.Monad.Writer
import           Data.Text   (Text)
import qualified Data.Text as Text

import Data.ByteString.Lazy(toStrict)
import qualified Data.Aeson as Aeson
import Data.Text.Encoding(decodeUtf8)

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

type Log = Text

type Model = Writer Log ()

data Action
    = NoOp
    | Show Log Text
    | QueryLog (Maybe UserId)
    deriving (Show)

echoBot :: BotApp Model Action
echoBot = BotApp 
    { botInitialModel = return ()
    , botAction = flip updateToAction
    , botHandler = handleAction
    , botJobs = []
    }

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ update =
    let logMessage = decodeUtf8 . toStrict . Aeson.encode $ update
        uId = userId <$> (updateMessage update >>= messageFrom)
        parser = 
                Show logMessage helpMessage <$  command "start"
            <|> Show logMessage helpMessage <$  command "help"
            <|> QueryLog uId                <$  command "log"
            <|> Show logMessage             <$> plainText
    in parseUpdate parser update

handleAction :: Action -> Model -> Eff Action Model
handleAction action model =
    let showmodel message log = model >>
            (tell log) >>
            (tell $ Text.pack "\n") >>
            (tell $ Text.append (Text.pack "Reply:\n") message)
        logmodel = return ()
        incorrectusermodel userId = model >>
            (tell $ Text.unwords
             [ "User with id"
             , Text.pack . show $ userId
             , "has tried to access logs.\n"
             ])
    in case action of
        NoOp -> pure model
        Show log message -> (showmodel message log) <# do
            reply (toReplyMessage message)
            return NoOp
        QueryLog (Just userId) -> if userId == ownerId
            then
                logmodel <# do
                    let msg = execWriter model
                    replyText msg
                    return NoOp
            else
                (incorrectusermodel userId) <# do
                    replyText accessDeniedMessage
                    return NoOp
        QueryLog _ -> model <# return NoOp

helpMessage :: Text
helpMessage = Text.unlines
    [ "Привет, я @myhlbot, и пока я ничего не умею."
    , "Можешь попробовать что-нибудь написать."
    ]

accessDeniedMessage :: Text
accessDeniedMessage = Text.unlines
    [ "Доступ к логам разрешён только разработчикам бота."
    ]

ownerId :: UserId
ownerId = UserId 205887307

run :: Token -> IO ()
run token = do
    env <- defaultTelegramClientEnv token
    startBot_ echoBot env

start :: IO ()
start = do
    token <- getEnvToken "TELEGRAM_BOT_TOKEN"
    run token

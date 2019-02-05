{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( start
    ) where

import Messages

import Control.Applicative
import Control.Monad.Writer
import           Data.Text   (Text)
import qualified Data.Text as Text

import Data.ByteString.Lazy(toStrict)
import qualified Data.Aeson as Aeson
import Data.Text.Encoding(decodeUtf8)

import Data.Hashable
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

type Log = Text
type Username = Text

type Model = Writer Log (HashMap UserId Chat)

instance Hashable UserId where
    hashWithSalt num (UserId uid) = hashWithSalt num uid

data Action
    = NoOp
    | Start (Maybe UserId) (Maybe Chat)
    | Show Log Text
    | QueryLog (Maybe UserId)
    deriving (Show)

echoBot :: BotApp Model Action
echoBot = BotApp 
    { botInitialModel = return HashMap.empty
    , botAction = flip updateToAction
    , botHandler = handleAction
    , botJobs = []
    }

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ update =
    let logMessage = decodeUtf8 . toStrict . Aeson.encode $ update
        uId = userId <$> (updateMessage update >>= messageFrom)
        chat = messageChat <$> updateMessage update
        parser = 
                Start uId chat              <$  command "start"
            <|> Show logMessage helpMessage <$  command "help"
            <|> QueryLog uId                <$  command "log"
            <|> Show logMessage pongMessage <$  command "ping"
    in parseUpdate parser update

handleAction :: Action -> Model -> Eff Action Model
handleAction action model =
    let showmodel message log = do
            users <- model
            tell log
            tell $ Text.pack "\n"
            tell $ Text.append (Text.pack "Reply:\n") message
            return users
        logmodel = botInitialModel echoBot
        incorrectusermodel userId = do
            users <- model
            tell $ accessMessage userId
            return users
        updatedmodel (Just uid) (Just chat) = HashMap.insert uid chat <$> model
        updatedmodel _ _ = model
    in case action of
        NoOp -> pure model
        Start uid chat -> (updatedmodel uid chat) <# do
            replyText helpMessage
            return NoOp
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

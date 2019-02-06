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

import Data.Maybe
import Data.Foldable

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
    | Start UserId Chat
    | Show Log Text
    | QueryLog UserId ChatType
    | Broadcast UserId Text
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
        uId = fromMaybe (UserId 0) . getUserId $ update
        chat = fromJust $ messageChat <$> updateMessage update
        writeMessage = cropCommand . fromJust . updateMessageText $ update
        parser = 
                Start uId chat               <$  command "start"
            <|> Show logMessage helpMessage  <$  command "help"
            <|> QueryLog uId (chatType chat) <$  command "log"
            <|> Show logMessage pongMessage  <$  command "ping"
            <|> Broadcast uId writeMessage   <$  command "broadcast"
    in parseUpdate parser update

handleAction :: Action -> Model -> Eff Action Model
handleAction action model =
    let showmodel message log = do
            chats <- model
            tell log
            tell $ Text.pack "\n"
            return chats
        logmodel = botInitialModel echoBot
        incorrectusermodel userId = do
            chats <- model
            tell $ accessMessage userId
            return chats
        updatedmodel uid chat = HashMap.insert uid chat <$> model
        broadcast userId message = do
            chats <- model
            tell $ broadcastMessage userId message
            return chats
    in case action of
        NoOp -> pure model
        Start uid chat -> (updatedmodel uid chat) <# do
            replyText helpMessage
            return NoOp
        Show log message -> (showmodel message log) <# do
            reply (toReplyMessage message)
            return NoOp
        QueryLog userId ChatTypePrivate -> if userId == ownerId
            then
                logmodel <# do
                    let (chats, msg) = runWriter model
                    replyText $ Text.append msg $ Text.pack . show $ chats
                    return NoOp
            else
                (incorrectusermodel userId) <# do
                    replyText accessDeniedMessage
                    return NoOp
        QueryLog _ _ -> model <# do
            replyText groupsDeniedMessage
            return NoOp
        Broadcast userId message -> if userId == ownerId
            then (broadcast userId message) <# do
                let (chats, _) = runWriter model
                    request chatId = defaultMessageRequest chatId message
                liftClientM . for_ chats $ \chat ->
                    sendMessage . request . chatId $ chat
                return NoOp
            else (broadcast userId message) <# do
                replyText broadcastDeniedMessage
                return NoOp

cropCommand :: Text -> Text
cropCommand = Text.unwords . tail . Text.words

getUserId :: Update -> Maybe UserId
getUserId update = userId <$> (updateMessage update >>= messageFrom)
            
ownerId :: UserId
ownerId = UserId 205887307

defaultMessageRequest :: ChatId -> Text -> SendMessageRequest
defaultMessageRequest id msg = SendMessageRequest (SomeChatId id) msg Nothing Nothing Nothing Nothing Nothing

run :: Token -> IO ()
run token = do
    env <- defaultTelegramClientEnv token
    startBot_ echoBot env

start :: IO ()
start = do
    token <- getEnvToken "TELEGRAM_BOT_TOKEN"
    run token

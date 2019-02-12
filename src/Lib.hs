{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Lib
    ( start
    ) where

import Hoogle
import Messages
import Wolfram

import Control.Applicative
import Control.Monad.Reader
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
    | Write Username Text
    | WriteToLog Text
    | Hoogle Int Text
    | Wolfram Text
    deriving (Show)

haskellBot :: BotApp Model Action
haskellBot = BotApp 
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
        broadcastMessage = cropFirstWord . fromJust . updateMessageText $ update
        croppedMessage = cropFirstWord . fromJust . updateMessageText $ update
        username = head . Text.words $ croppedMessage
        count = (read . Text.unpack $ username) :: Int
        writeMessage = cropFirstWord croppedMessage
        parser = 
                Start uId chat                 <$  command "start"
            <|> Show logMessage helpMessage    <$  command "help"
            <|> QueryLog uId (chatType chat)   <$  command "log"
            <|> Show logMessage pongMessage    <$  command "ping"
            <|> Broadcast uId broadcastMessage <$  command "broadcast"
            <|> Write username writeMessage    <$  command "write"
            <|> Hoogle count writeMessage      <$  command "hoogle"
            <|> Wolfram croppedMessage         <$  command "wolfram"
            <|> Wolfram croppedMessage         <$  command "wf"
    in parseUpdate parser update

handleAction :: Action -> Model -> Eff Action Model
handleAction action model =
    let writetologmodel message = do
            chats <- model
            tell $ Text.cons '\n' message
            return chats
    in case action of
        NoOp -> pure model
        Start uid chat -> handleStart uid chat model
        Show log message -> handleShow log message model
        QueryLog userId chatType -> handleQuery userId chatType model
        Broadcast userId message -> handleBroadcast userId message model
        Write username message -> handleWrite username message model
        WriteToLog message -> (writetologmodel message) <# return NoOp
        Hoogle count query -> handleHoogle count query model
        Wolfram query -> handleWolfram query model

handleStart :: UserId -> Chat -> Model -> Eff Action Model
handleStart userId chat model = newmodel <# action
    where
        newmodel = HashMap.insert userId chat <$> model
        action = do
            replyText helpMessage
            return NoOp

handleShow :: Log -> Text -> Model -> Eff Action Model
handleShow log message model = newmodel <# action
    where
        newmodel = do
            chats <- model
            tell log
            tell $ Text.pack "\n"
            return chats
        action = do
            reply (toReplyMessage message)
            return NoOp

handleQuery :: UserId -> ChatType -> Model -> Eff Action Model
handleQuery userId ChatTypePrivate model | userId == ownerId = newmodel <# replyLogAction
                                         | otherwise = accessdeniedmodel <# denyAccessAction
    where
        newmodel = censor (\_ -> "") model
        accessdeniedmodel = do
            chats <- model
            tell $ accessMessage userId
            return chats
        replyLogAction = do 
            let (chats, msg) = runWriter model
            replyText $ Text.append msg $ Text.pack . show $ chats
            return NoOp
        denyAccessAction = do
            replyText accessDeniedMessage
            return NoOp    
handleQuery _      _               model = model <# groupsDenyAction
    where
        groupsDenyAction = do
            replyText groupsDeniedMessage
            return NoOp

handleWrite :: Username -> Text -> Model -> Eff Action Model
handleWrite username message model = model <# do
    let request = SendMessageRequest (SomeChatUsername username) message Nothing Nothing Nothing Nothing Nothing
    response <- liftClientM . sendMessage $ request
    return . WriteToLog . Text.pack . show $ response

handleBroadcast :: UserId -> Text -> Model -> Eff Action Model
handleBroadcast userId message model = newmodel <# action
    where
        newmodel = do
            chats <- model
            tell $ broadcastMessage userId message
            return chats
        action = do
            if userId == ownerId
                then do
                    let (chats, _) = runWriter model
                        request chatId = defaultMessageRequest chatId message
                    liftClientM . for_ chats $ \chat ->
                        sendMessage . request . chatId $ chat
                    return NoOp
                else do
                    replyText broadcastDeniedMessage
                    return NoOp

handleHoogle :: Int -> Text -> Model -> Eff Action Model
handleHoogle count query model = newmodel <# action
    where
        newmodel = do
            chats <- model
            tell $ Text.pack "Hoogle search: "
            tell query
            tell "\n"
            return chats
        action = do
            res <- liftIO $ search query count
            case res of
                Left err -> do
                    replyText hoogleFailMessage
                    return $ WriteToLog err
                Right results -> do
                    let messages = map printResult results
                    replyText $ Text.pack $ "Showing " ++ (show count) ++ " results."
                    mapM_ replyText messages
                    return $ WriteToLog $ Text.unlines messages

handleWolfram :: Text -> Model -> Eff Action Model
handleWolfram query model = newmodel <# action
    where
        newmodel = do
            chats <- model
            tell $ Text.pack "Wolfram search: "
            tell query
            tell "\n"
            return chats
        action = do
            url <- liftIO . wolfram $ query
            chatId <- currentChatId
            case chatId of
                Just chatId -> do
                    let request = formSendDocumentRequest chatId (Text.pack url)
                    response <- liftClientM . sendDocument $ request
                    return $ WriteToLog . Text.pack . show $ response
                _ -> return $ WriteToLog . Text.pack $ "Failed to find current chat\n"

cropFirstWord :: Text -> Text
cropFirstWord = Text.unwords . tail . Text.words

getUserId :: Update -> Maybe UserId
getUserId update = userId <$> (updateMessage update >>= messageFrom)
            
ownerId :: UserId
ownerId = UserId 205887307

formSendDocumentRequest :: ChatId -> Text -> SendDocumentRequest
formSendDocumentRequest id doc = SendDocumentRequest (SomeChatId id) doc Nothing Nothing Nothing Nothing Nothing

defaultMessageRequest :: ChatId -> Text -> SendMessageRequest
defaultMessageRequest id msg = SendMessageRequest (SomeChatId id) msg Nothing Nothing Nothing Nothing Nothing

printResult :: Result -> Text
printResult result = Text.unlines . map Text.pack $
    [ self result
    , docs result
    , location result
    ]

run :: Token -> IO ()
run token = do
    env <- defaultTelegramClientEnv token
    startBot_ haskellBot env

start :: IO ()
start = do
    token <- getEnvToken "TELEGRAM_BOT_TOKEN"
    run token

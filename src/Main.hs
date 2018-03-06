{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Data.Text (Text)
import qualified Data.Text as Text

import Telegram.Bot.API
import Telegram.Bot.Simple

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.Types

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where
    fromRow = TestField <$> field <*> field

type Model = ()

data Action
    = NoOp
    | Echo Text

type Task = String

echoBot :: BotApp Model Action
echoBot = BotApp
    { botInitialModel = ()
    , botAction = updateToAction
    , botHandler = handleAction
    , botJobs = []
    }
    where
        updateToAction :: Update -> Model -> Maybe Action
        updateToAction update _ =
            case updateMessageText update of
                Just text -> Just (Echo text)
                Nothing   -> Nothing

        handleAction :: Action -> Model -> Eff Action Model
        handleAction action model = case action of
            NoOp -> pure model
            Echo msg -> model <# do
                replyText . Text.pack $ "Hi! I'm dumb."
                return NoOp

run :: Token -> IO ()
run token = do
    env <- defaultTelegramClientEnv token
    startBot_ (conversationBot updateChatId echoBot) env

saveTask :: Task ->

main :: IO ()
main = do
    conn <- open "test.db"
    close conn
    execute conn "INSERT INTO test (str) VALUES (?)"
        (Only ("test string 2" :: String))
    r <- query_ conn "SELECT * from test" :: IO [TestField]
    token <- getEnv $ "TELEGRAM_TOKEN"
    run . Token . Text.pack $ token

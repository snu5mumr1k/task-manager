{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

module Main where

import System.Environment
import Data.Text (Text)
import qualified Data.Text as Text

import Telegram.Bot.API
import Telegram.Bot.Simple

type Model = ()

data Action
  = NoOp
  | Echo Text

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

main :: IO ()
main = do
    token <- getEnv $ "TELEGRAM_TOKEN"
    run . Token . Text.pack $ token

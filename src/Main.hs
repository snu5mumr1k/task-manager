{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Task
import TasksStorage

import Control.Applicative
import Control.Monad.IO.Class
import Data.Text.IO as TextIO
import GHC.Exts hiding ((<#))
import System.Environment
import qualified Data.Text as Text
import Text.Printf

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

data Action
  = NoOp
  | Start
  | AddTask Task.Text
  | RemoveTask Task.Text
  | Show
  deriving (Show, Read)

data Model = Model TasksStorage

databaseName :: String
databaseName = "test.db"

taskManager :: BotApp Model Action
taskManager = BotApp
  { botInitialModel = Model $ getTasksStorage databaseName
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }
  where
    updateToAction :: Model -> Update -> Maybe Action
    updateToAction _ = parseUpdate $
          AddTask <$> plainText
      <|> Start <$  command "start"
      <|> AddTask <$> command "add"
      <|> RemoveTask <$> command "remove"
      <|> Show <$ command "show"
      <|> callbackQueryDataRead

    handleAction :: Action -> Model -> Eff Action Model
    handleAction action (Model tasksStorage) = case action of
      NoOp -> pure $ Model tasksStorage
      Start -> Model tasksStorage <# do
        reply "Start"
        return NoOp
      AddTask text -> Model tasksStorage <# do
        taskId <- liftIO $ addTask tasksStorage $ Task.readTask text
        task <- liftIO $ getTask tasksStorage taskId
        replyText . Text.pack $ printf "Added new task:\n%s" $ show task
        return NoOp
      RemoveTask "" -> Model tasksStorage <# do
        liftIO $removeAllTasks tasksStorage
        replyText $ "Tasks list cleared"
        return NoOp
      RemoveTask stringifiedTaskId -> Model tasksStorage <# do
        task <- liftIO $ getTask tasksStorage taskId
        liftIO $ removeTask tasksStorage taskId
        replyText . Text.pack $ printf "Task removed:\n%s" $ show task
        return NoOp
        where
          taskId = read $ Text.unpack stringifiedTaskId
      Show -> Model tasksStorage <# do
        tasks <- liftIO $ getAllTasks tasksStorage
        case tasks of
          [] -> replyText $ "There are no tasks"
          tasks -> replyText . Text.intercalate "\n-------\n" $ map (Text.pack . show) tasks
        return NoOp

runBot :: Token -> IO ()
runBot token = do
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId taskManager) env

main :: IO ()
main = do
  setupDatabase $ Database databaseName
  token <- getEnv $ "TELEGRAM_TOKEN"
  runBot . Token . Text.pack $ token

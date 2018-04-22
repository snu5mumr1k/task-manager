{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text.IO as TextIO
import GHC.Int
import System.Environment
import qualified Data.Text as Text

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.Types
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

database :: String
database = "test.db"

type Task = Text
type TaskId = GHC.Int.Int64

data Database = Database String

data Action
  = NoOp
  | Start
  | AddTask Task
  | RemoveTask Task
  | Show
  deriving (Show, Read)

data TasksStorage = TasksStorage
  { addTask :: Task -> IO TaskId
  , removeTask :: TaskId -> IO ()
  , getTask :: TaskId -> IO Task
  , getAllTasks :: IO [Task]
  }

data Model = Model TasksStorage

getTasksStorage = TasksStorage
  { addTask = addTask_
  , removeTask = removeTask_
  , getTask = getTask_
  , getAllTasks = getAllTasks_
  }
  where
    addTask_ task = do
      conn <- open database
      execute conn "insert into Tasks (Text) values(?)" (Only task)
      taskId <- lastInsertRowId conn
      close conn
      return taskId

    removeTask_ taskId = do
      conn <- open database
      execute conn "delete from Tasks where TaskId = (?)" (Only taskId)
      close conn

    getTask_ taskId = do
      conn <- open database
      [Only result] <- query conn "select Text from Tasks where TaskId = (?)" (Only taskId) :: IO [Only Task]
      close conn
      return result

    getAllTasks_ = do
      conn <- open database
      result <- query_ conn "select Text from Tasks" :: IO [Only Task]
      close conn
      return $ map fromOnly result

setupDatabase :: Database -> IO ()
setupDatabase (Database database) = do
  conn <- open database
  execute_ conn "create table if not exists Tasks (TaskID integer primary key, Text TEXT)"
  close conn

taskManager :: BotApp Model Action
taskManager = BotApp
  { botInitialModel = Model getTasksStorage
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
      NoOp -> pure (Model tasksStorage)
      Start -> Model tasksStorage <# do
        reply "Start"
        return NoOp
      AddTask task -> Model tasksStorage <# do
        taskId <- liftIO $ addTask tasksStorage task
        reply . toReplyMessage . Text.pack $ "Added " ++ (show taskId)
        return NoOp
      RemoveTask task -> Model tasksStorage <# do
        reply "Task removed"
        return NoOp
      Show -> Model tasksStorage <# do
        tasks <- liftIO $ getAllTasks tasksStorage
        reply . toReplyMessage . Text.concat $ ["Show \n", (Text.unlines tasks)]
        return NoOp

runBot :: Token -> IO ()
runBot token = do
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId taskManager) env

main :: IO ()
main = do
  setupDatabase (Database database)
  token <- getEnv $ "TELEGRAM_TOKEN"
  runBot . Token . Text.pack $ token

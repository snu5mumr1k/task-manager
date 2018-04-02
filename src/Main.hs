{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import GHC.Int
import System.Environment
import Data.Text (Text)
import Data.Text.IO as TextIO
import qualified Data.Text as Text

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.Types

database :: String
database = "test.db"

type Task = Text
type TaskId = GHC.Int.Int64

data Database = Database String

data Model = Model

data Action
  = NoOp
  | Start
  | AddTask Task
  | RemoveTask Task
  | Show Text
  deriving (Show, Read)

data TasksStorage = TasksStorage
  { addTask :: Task -> IO TaskId
  , removeTask :: TaskId -> IO ()
  , showTask :: TaskId -> IO Task
  }

getTasksStorage = TasksStorage
  { addTask = addTask_
  , removeTask = removeTask_
  , showTask = showTask_
  }
  where
    addTask_ task = do
      conn <- open database
      execute conn "insert into Tasks (Text) values(?)"
        (Only task)
      taskId <- lastInsertRowId conn
      close conn
      return taskId

    removeTask_ taskId = do
      conn <- open database
      execute conn "delete from Tasks where TaskId = (?)" (Only taskId)
      close conn

    showTask_ taskId = do
      conn <- open database
      [Only result] <- query conn "select Text from Tasks where TaskId = (?)" (Only taskId) :: IO [Only Task]
      close conn
      return result

setupDatabase :: Database -> IO ()
setupDatabase (Database database) = do
  conn <- open database
  execute_ conn "create table if not exists Tasks (TaskID integer primary key, Text TEXT)"
  close conn

taskManager :: BotApp Model Action
taskManager = BotApp
  { botInitialModel = Model
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }
  where
    updateToAction :: Model -> Update -> Maybe Action
    updateToAction _ = parseUpdate $
          AddTask      <$> plainText
      <|> Start        <$  command "start"
      <|> AddTask      <$> command "add"
      <|> RemoveTask   <$> command "remove"
      <|> Show         <$> command "show"
      <|> callbackQueryDataRead

    handleAction :: Action -> Model -> Eff Action Model
    handleAction action model = case action of
      NoOp -> pure model
      Start -> model <# do
        reply "Start"
        return NoOp
      AddTask task -> model <# do
        reply "Add"
        return NoOp
      RemoveTask task -> model <# do
        reply "Remove"
        return NoOp
      Show task -> model <#do
        reply "Show"
        return NoOp

run :: TasksStorage -> Task -> IO Task
run (TasksStorage {showTask, removeTask, addTask}) task = do
  taskId <- addTask task
  showTask taskId

runBot :: Token -> IO ()
runBot token = do
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId taskManager) env

main :: IO ()
main = do
  setupDatabase (Database database)
  inputText <- TextIO.getLine
  textInBase <- run (getTasksStorage) inputText
  TextIO.putStrLn textInBase
  token <- getEnv $ "TELEGRAM_TOKEN"
  runBot . Token . Text.pack $ token

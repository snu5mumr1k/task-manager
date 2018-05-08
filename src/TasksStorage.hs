{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module TasksStorage where

import GHC.Int
import qualified Data.Text as Text

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.Types

type Task = Text.Text
type TaskId = GHC.Int.Int64

data Database = Database String

data TasksStorage = TasksStorage
  { addTask :: Task -> IO TaskId
  , removeTask :: TaskId -> IO Bool
  , removeAllTasks :: IO Bool
  , getTask :: TaskId -> IO Task
  , getAllTasks :: IO [(TaskId, Task)]
  }

getTasksStorage = TasksStorage
  { addTask = addTask_
  , removeTask = removeTask_
  , removeAllTasks = removeAllTasks_
  , getTask = getTask_
  , getAllTasks = getAllTasks_
  }
  where
    databaseName = "test.db"

    addTask_ task = do
      conn <- open databaseName
      execute conn "insert into Tasks (Text) values(?)" (Only task)
      taskId <- lastInsertRowId conn
      close conn
      return taskId

    removeTask_ taskId = do
      conn <- open databaseName
      execute conn "delete from Tasks where TaskId = (?)" (Only taskId)
      close conn
      return True

    removeAllTasks_ = do
      conn <- open databaseName
      execute_ conn "delete from Tasks"
      close conn
      return True

    getTask_ taskId = do
      conn <- open databaseName
      [Only result] <- query conn "select Text from Tasks where TaskId = (?)" (Only taskId) :: IO [Only Task]
      close conn
      return result

    getAllTasks_ = do
      conn <- open databaseName
      result <- query_ conn "select TaskId, Text from Tasks" :: IO [(TaskId, Task)]
      close conn
      return result

setupDatabase :: Database -> IO ()
setupDatabase (Database databaseName) = do
  conn <- open databaseName
  execute_ conn "create table if not exists Tasks (TaskID integer primary key, Text TEXT)"
  close conn

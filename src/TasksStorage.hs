{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module TasksStorage where

import qualified Task

import Database.SQLite.Simple

data Database = Database String

data TasksStorage = TasksStorage
  { addTask :: Task.Text -> IO Task.Id
  , removeTask :: Task.Id -> IO Bool
  , removeAllTasks :: IO Bool
  , getTask :: Task.Id -> IO Task.Text
  , getAllTasks :: IO [Task.Task]
  }

getTasksStorage databaseName = TasksStorage
  { addTask = addTask_
  , removeTask = removeTask_
  , removeAllTasks = removeAllTasks_
  , getTask = getTask_
  , getAllTasks = getAllTasks_
  }
  where
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
      [Only result] <- query conn "select Text from Tasks where TaskId = (?)" (Only taskId) :: IO [Only Task.Text]
      close conn
      return result

    getAllTasks_ = do
      conn <- open databaseName
      result <- query_ conn "select TaskId, Text, AlarmTime from Tasks" :: IO [Task.Task]
      close conn
      return result

setupDatabase :: Database -> IO ()
setupDatabase (Database databaseName) = do
  conn <- open databaseName
  execute_ conn "create table if not exists Tasks (TaskID integer primary key, Text text not null, AlarmTime text default \"2018-05-10 12:12:12\")"
  close conn

{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module TasksStorage where

import qualified Task

import Database.SQLite.Simple

data Database = Database String

data TasksStorage = TasksStorage
  { addTask :: Task.Task -> IO Task.Id
  , removeTask :: Task.Id -> IO ()
  , removeAllTasks :: IO ()
  , getTask :: Task.Id -> IO Task.Task
  , getAllTasks :: IO [Task.Task]
  }

getTasksStorage :: String -> TasksStorage
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
      execute conn "insert into Tasks (Text, AlarmTime) values(?, ?)" task
      taskId <- lastInsertRowId conn
      close conn
      return taskId

    removeTask_ taskId = do
      conn <- open databaseName
      execute conn "delete from Tasks where TaskId = (?)" (Only taskId)
      close conn
      return ()

    removeAllTasks_ = do
      conn <- open databaseName
      execute_ conn "delete from Tasks"
      close conn
      return ()

    getTask_ taskId = do
      conn <- open databaseName
      [result] <- query conn "select TaskId, Text, AlarmTime from Tasks where TaskId = (?)" (Only taskId) :: IO [Task.Task]
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

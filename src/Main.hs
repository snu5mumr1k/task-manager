{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import GHC.Int
import System.Environment
import Data.Text (Text)
import qualified Data.Text as Text

import Telegram.Bot.API
import Telegram.Bot.Simple

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.Types

database :: String
database = "test.db"

type Task = String
type TaskId = GHC.Int.Int64

data TaskManager = TaskManager
    { addTask :: Task -> IO TaskId
    , removeTask :: TaskId -> IO ()
    , showTask :: TaskId -> IO Task
    }

getTaskManager = TaskManager
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
            [Only result] <- query conn "select Text from Tasks where TaskId = (?)" (Only taskId) :: IO [Only String]
            close conn
            return result

createTable :: String -> IO ()
createTable database= do
    conn <- open database
    execute_ conn "create table if not exists Tasks (TaskID integer primary key, Text TEXT)"
    close conn

run :: TaskManager -> Task -> IO Task
run (TaskManager {showTask, removeTask, addTask}) task = do
    taskId <- addTask task
    showTask taskId

main :: IO ()
main = do
    createTable database
    inputText <- getLine
    textInBase <- run (getTaskManager) inputText
    putStrLn textInBase

{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Task where

import Data.Maybe
import Data.Time.Format
import GHC.Int
import Text.Printf
import qualified Data.Text as Text
import qualified Data.Time as Time

import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow

type Id = GHC.Int.Int64
type Text = Text.Text
type AlarmTime = Time.UTCTime

data Task = Task Id Text AlarmTime

instance Show Task where
  show (Task id text alarmTime) = printf "Task Id: %d\nContent: %s\nAlarm Time: %s" id (Text.unpack text) (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" alarmTime)

instance FromRow Task where
  fromRow = Task <$> field <*> field <*> field

instance ToRow Task where
  toRow (Task _ text alarmTime) = toRow (text, alarmTime)

readTask :: Text -> Task
readTask str = Task id text alarmTime
  where
    parts = Text.splitOn "|" str
    id = 0
    text = Text.strip $ last parts
    alarmTime = fromJust $ parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" $ Text.unpack $ head parts

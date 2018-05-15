{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Task where

import GHC.Int
import qualified Data.Text as Text
import qualified Data.Time as Time
import Data.Time.Format
import Text.Printf

import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow

type Id = GHC.Int.Int64
type Text = Text.Text
type AlarmTime = Time.UTCTime

data Task = Task Id Text AlarmTime

instance Show Task where
  show (Task id text alarmTime) = printf "%d: %s %s" id (Text.unpack text) (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" alarmTime)

instance FromRow Task where
  fromRow = Task <$> field <*> field <*> field

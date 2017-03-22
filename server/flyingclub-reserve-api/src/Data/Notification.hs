{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Data.Notification (OutputTarget(..), Context(..), parsedActionResultResponse) where

import           Control.Monad.Reader
import           Data.ParsedActionResult
import           Data.Semigroup
import           Data.String.Interpolate
import qualified Data.Text                           as T
import           Data.Time.Friendly
import           Data.Time.LocalTime.TimeZone.Series
import           Database.Persist.Schema
import           Database.Persist.Sql

data OutputTarget = SMS | Voice
data Context = Context
  { target    :: OutputTarget
  , zst       :: ZoneSeriesTime
  , airplanes :: [Entity Airplane]
  , users     :: [Entity User]
  }

userName :: Key User -> Reader Context T.Text
userName userId = asks $ (\u->userFirstname u <> " " <> userLastname u).entityVal.head.(filter (\u->userId == entityKey u)).users

parsedActionResultResponse :: ParsedActionResult -> Reader Context T.Text
parsedActionResultResponse par = case par of
  CheckResult [] -> return "The airplane is available for the entire day"
  CheckResult reses -> do
    entries <- mapM (\(Entity _ res) -> do
        name <- userName $ reservationUserId res
        zst <- asks zst
        return $ name <> " on " <> formatZSTUTCPair zst (reservationStart res) (reservationEnd res)
      ) reses
    return $ "The airplane is already scheduled by " <> (T.intercalate ", and by " entries)

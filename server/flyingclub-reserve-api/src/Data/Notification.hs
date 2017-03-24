{-# LANGUAGE FlexibleContexts  #-}
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

find :: Eq (Key a) => Key a -> (Context -> [Entity a]) -> Context -> a
find k es = entityVal.head.(filter (\u->k == entityKey u)).es

userName :: Key User -> Reader Context T.Text
userName userId = asks $ (\u->userFirstname u <> " " <> userLastname u).find userId users

airplane :: Key Airplane -> Reader Context T.Text
airplane airplaneId = asks $ (\a->airplaneTail a <> " " <> airplaneDescription a).find airplaneId airplanes

parsedActionResultResponse :: ParsedActionResult -> Reader Context T.Text
parsedActionResultResponse par = case par of
  CheckResult [] -> return "This airplane is available for the entire day"
  CheckResult reses -> do
    entries <- mapM (\(Entity _ res) -> do
        name <- userName $ reservationUserId res
        zst <- asks zst
        return $ name <> " " <> formatZSTUTCPair zst (reservationStart res) (reservationEnd res)
      ) reses
    return $ "The airplane is scheduled by " <> (T.intercalate ", and by " entries)

  ReviewResult [] -> return "You have no reservations for the time period"
  ReviewResult reses -> do
    entries <- mapM (\(Entity _ res) -> do
        plane <- airplane $ reservationAirplaneId res
        zst <- asks zst
        return $ plane <> " " <> formatZSTUTCPair zst (reservationStart res) (reservationEnd res)
      ) reses
    return $ "You're reserved in " <> (T.intercalate ", and in " entries)

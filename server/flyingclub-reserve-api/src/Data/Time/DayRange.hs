{-# LANGUAGE DeriveGeneric #-}
module Data.Time.DayRange where

import           Data.Aeson
import qualified Data.Text                           as T
import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate
import           Data.Time.Clock
import           Data.Time.Friendly
import           Data.Time.LocalTime
import           Data.Time.LocalTime.TimeZone.Series
import qualified GHC.Generics                        as G

data TimeOfDaySlot = TimeOfDaySlot
  { slotUtc     :: UTCTime
  , slotLocal   :: LocalTime
  , slotDisplay :: T.Text
  } deriving (G.Generic)

instance ToJSON TimeOfDaySlot

mkTimeOfDaySlot :: ZoneSeriesTime -> TimeOfDaySlot
mkTimeOfDaySlot zst = TimeOfDaySlot
  (zoneSeriesTimeToUTC zst)
  local
  (formatTime $ localTimeOfDay local)
  where local = zoneSeriesTimeToLocalTime zst

dayRange :: TimeZoneSeries -> Day -> (UTCTime, UTCTime)
dayRange tzs localDay = (lt2utc midnight, lt2utc (TimeOfDay 23 59 59))
  where lt2utc tod = localTimeToUTC' tzs (LocalTime localDay tod)

enumerateLocalHours :: TimeZoneSeries -> Day -> [TimeOfDaySlot]
enumerateLocalHours tzs localDay = [mkTimeOfDaySlot (ZoneSeriesTime hour tzs) | hour <- range]
  where
    (begin, end) = dayRange tzs localDay
    range = takeWhile ((>) end) $ iterate (addUTCTime (60*60)) begin
      -- [begin, addUTCTime (60*60) begin..end] no instance for Enum UTCTime
      -- could convert to seconds, enum, then back via POSIX module...

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Time.Friendly (formatDay, formatTime) where

import           Data.String.Interpolate
import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate
import           Data.Time.Format            (TimeLocale, defaultTimeLocale,
                                              months, wDays)
import           Data.Time.LocalTime

{--
i considered using Data.Time.Format but it didn't have the support
I wanted, such as non-padded days and th, nd, st
So I implemented myself
--}

trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- Array is Sun = 0, Mon = 1 but the gregorian is Mon =1 ... Sun = 7
-- this obviously makes a problem for Sunday
lookupFst :: (TimeLocale -> [(a, b)]) -> Int -> a
lookupFst f wd = case wd of
  7 -> head weekdays
  _ -> weekdays !! wd
  where weekdays = map fst (f defaultTimeLocale)


year :: Day -> Integer
year = fst3.toGregorian

formatWeekDay :: Day -> String
formatWeekDay = (lookupFst wDays).trd3.toWeekDate

-- for month, the gregorian months are 1..12 but the array is Jan.. where 0 = Jan
formatMonth :: Day -> String
formatMonth = (lookupFst months).(\x -> x-1).snd3.toGregorian

ordinal :: Int -> String
ordinal n | n > 10 && n < 20 = "th"
          | otherwise =
            case n `mod` 10 of
              1 -> "st"
              2 -> "nd"
              3 -> "rd"
              _ -> "th"

formatDayN :: Day -> String
formatDayN d = [i|#{dayn}#{ordinal dayn}|]
  where dayn = (trd3.toGregorian) d


formatDay :: Day -> Day -> String
formatDay today day   | today == day              = "today"
                      | addDays 1 today == day    = "tomorrow"
                      | year today == year day    = [i|#{formatWeekDay day}, #{formatMonth day} #{formatDayN day}|]
                      | otherwise                 = [i|#{formatWeekDay day}, #{formatMonth day} #{formatDayN day}, #{year day}|]

formatTime :: TimeOfDay -> String
formatTime tod@TimeOfDay{..} = [i|#{hour}:#{minutePad}#{todMin} #{ampm}|]
  where
    hour = case todHour `mod` 12 of
      0 -> 12
      x -> x
    minutePad = if todMin < 10 then "0" else ""
    ampm = if todHour < 12 then "AM" else "PM"

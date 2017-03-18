{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Request.Parser.Time (timeFromNow, timeFromNowWithDefaultDay) where

import           Control.Applicative
import           Control.Exception.Format
import           Control.Exception.StackError
import           Data.Attoparsec.Text
import qualified Data.Text                           as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.LocalTime.TimeZone.Series
import           Request.Parser.Day
import           Request.Parser.Utility

h12 :: Int -> Int
h12 hour = if hour == 12 then 0 else hour

am :: Parser T.Text
am = "am" <|> "a"

pm :: Parser T.Text
pm = "pm" <|> "p"

tod :: Parser Int -> Parser T.Text -> Parser TimeOfDay
tod hours term =
    TimeOfDay <$> hours <*> (":" *> digits 2 59) <*> pure 0 <* term <|>
    TimeOfDay <$> hours <*> digits 2 59 <*> pure 0 <* term <|>
    TimeOfDay <$> hours <*> pure 0 <*> pure 0 <* term

time :: Parser TimeOfDay
time =
    tod (h12 <$> digits 2 12) am <|>
    tod (h12 <$> digits 1 11) am <|>
    tod ((+12) <$> h12 <$> digits 2 12) pm <|>
    tod ((+12) <$> h12 <$> digits 1 11) pm <|>
    tod (digits 2 23) ""


localTimeFromNow :: Parser (ZoneSeriesTime -> Maybe Day -> UTCTime)
localTimeFromNow = do
    d <- option Nothing (Just <$> dayFromToday)
    skipSpace
    t <- time
    return $ \zonedSeriesTime defaultDay -> let
        startDay = (localDay.zoneSeriesTimeToLocalTime) zonedSeriesTime
        tz = zoneSeriesTimeSeries zonedSeriesTime
        d' = case (d, defaultDay) of
          (Nothing, Nothing)         -> id  -- if no day given to parse, and no default, it's the current day
          (Nothing, Just defaultDay) -> const defaultDay -- ignore the current day and use the default
          (Just givenDay, _)         -> givenDay
        local = LocalTime (d' startDay) t
        in
        if isValidLocalTime tz local
        then localTimeToUTC' tz local
        else throw $ FormatException "Invalid local time"

utcTimeFromNow :: Parser (ZoneSeriesTime -> Maybe Day -> UTCTime)
utcTimeFromNow = do
    day <- digits 2 31
    hour <- digits 2 23
    minute <- digits 2 59
    asciiCI "z"
    return $ \(toGregorian.utctDay.zoneSeriesTimeToUTC -> (startYear, startMonth, startDay)) _ ->
        case
            if day >= startDay
            then fromGregorianValid startYear startMonth day
            else if startMonth < 12
                then fromGregorianValid startYear (startMonth + 1) day
                else fromGregorianValid (startYear + 1) 1 day
        of
        Just actualDay -> UTCTime actualDay (secondsToDiffTime.toInteger $ hour*60*60 + minute*60)
        Nothing -> throw $ FormatException "Invalid date"

-- default day is used if there is no day provided in the parse but we want to use a different day than the zst day
timeFromNowWithDefaultDay :: Parser (ZoneSeriesTime -> Maybe Day -> UTCTime)
timeFromNowWithDefaultDay = utcTimeFromNow <|> localTimeFromNow

timeFromNow :: Parser (ZoneSeriesTime -> UTCTime)
timeFromNow = flip <$> timeFromNowWithDefaultDay <*> return Nothing

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Request.Parser.Time (timeFromNow) where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Series
import Request.Parser.Day
import Request.Parser.Utility

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
    

localTimeFromNow :: Parser (ZoneSeriesTime -> UTCTime)
localTimeFromNow = do
    d <- dayFromToday
    skipSpace
    t <- time
    return $ \zonedSeriesTime -> let
        startDay = (localDay.zoneSeriesTimeToLocalTime) zonedSeriesTime
        tz = zoneSeriesTimeSeries zonedSeriesTime
        local = LocalTime (d startDay) t
        in
        if isValidLocalTime tz local 
        then localTimeToUTC' tz local 
        else error "Invalid local time"
 
utcTimeFromNow :: Parser (ZoneSeriesTime -> UTCTime)
utcTimeFromNow = do
    day <- digits 2 31
    hour <- digits 2 23
    minute <- digits 2 59
    _ <- string "z"
    return $ \(toGregorian.utctDay.zoneSeriesTimeToUTC -> (startYear, startMonth, startDay)) -> 
        case
            if day >= startDay
            then fromGregorianValid startYear startMonth day
            else if startMonth < 12 
                then fromGregorianValid startYear (startMonth + 1) day
                else fromGregorianValid (startYear + 1) 1 day
        of
        Just actualDay -> UTCTime actualDay (secondsToDiffTime.toInteger $ hour*60*60 + minute*60)
        Nothing -> error "Invalid date"

timeFromNow :: Parser (ZoneSeriesTime -> UTCTime)
timeFromNow = localTimeFromNow <|> utcTimeFromNow

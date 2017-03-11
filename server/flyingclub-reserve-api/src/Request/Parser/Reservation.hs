{-# LANGUAGE OverloadedStrings #-}
module Request.Parser.Reservation (actionFromText) where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char
import           Data.ParsedAction
import           Data.Text
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Data.Time.LocalTime.TimeZone.Series
import           Request.Parser.Day
import           Request.Parser.Time

tailNumber :: Parser Text
tailNumber = takeWhile1 isAlphaNum

untilp :: Parser Text
untilp = choice $ Prelude.map asciiCI ["to ", "till ", "til ", "until "] -- we must have space after to to avoid "to-day"

reserve :: Parser (ZoneSeriesTime -> ParsedAction)
reserve = do
  asciiCI "reserve"
  skipSpace
  aid <- tailNumber
  skipSpace
  stime <- timeFromNow
  skipSpace
  option "" (untilp <* skipSpace)
  etime <- timeFromNow
  return $ \zonedSeriesTime ->
    Reserve aid (stime zonedSeriesTime) (etime zonedSeriesTime)

check :: Parser (ZoneSeriesTime -> ParsedAction)
check = do
  asciiCI "check"
  skipSpace
  aid <- tailNumber
  skipSpace
  sday <- option id dayFromToday
  return $ \zonedSeriesTime -> let startDay = (localDay.zoneSeriesTimeToLocalTime) zonedSeriesTime in
    Check aid (sday startDay)



actionFromText :: Parser (ZoneSeriesTime -> ParsedAction)
actionFromText = reserve <|> check

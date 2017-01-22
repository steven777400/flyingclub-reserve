{-# LANGUAGE OverloadedStrings #-}
module Request.Parser.TimeSpec where

import Control.Applicative
import Data.Attoparsec.Text

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Data.Time.LocalTime.TimeZone.Series
import Data.Time.LocalTime.TimeZone.Olson


import Test.Hspec
import Request.Parser.Time

-- 57445 = a saturday, 2/27/16, by utc
-- in this timezone, we expect that 0227 0000 utc -> 2/27 4pm
-- because (4+12) + 8 = 24 -> 00
originDay = ModifiedJulianDay 57445
utcOrigin = UTCTime originDay 0
-- so utcOrigin is 2/27/16 4pm


getzst = do
    tzs <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/America/Los_Angeles"
    return $ ZoneSeriesTime utcOrigin tzs




testTime zst str timestr = do
    let day = parseOnly (timeFromNow <* endOfInput) str
    case day of
        Left err -> error err
        Right f -> show $ f zst
    `shouldBe` timestr

spec :: Spec
spec = do
    describe "timeFromNow" $ do
        it "identify today time" $ do
            zst <- getzst
            testTime zst "today 4pm" "2016-02-27 00:00:00 UTC"
            testTime zst "today 5pm" "2016-02-27 01:00:00 UTC" -- moving forward
            testTime zst "today 530pm" "2016-02-27 01:30:00 UTC"
            testTime zst "today 10:30pm" "2016-02-27 06:30:00 UTC"
            testTime zst "today 11:59pm" "2016-02-27 07:59:00 UTC"
        it "identify tomorrow time" $ do
            zst <- getzst
            testTime zst "tomorrow 12:00am" "2016-02-27 08:00:00 UTC"
            testTime zst "tomorrow 1:00am" "2016-02-27 09:00:00 UTC"
            testTime zst "tomorrow 11:00am" "2016-02-27 19:00:00 UTC"
            testTime zst "tomorrow 11:59am" "2016-02-27 19:59:00 UTC"
            testTime zst "tomorrow 12:00pm" "2016-02-27 20:00:00 UTC"
            testTime zst "tomorrow 3:59pm" "2016-02-27 23:59:00 UTC"
            testTime zst "tomorrow 4pm" "2016-02-28 00:00:00 UTC"
            testTime zst "tomorrow 5pm" "2016-02-28 01:00:00 UTC"
        it "identify 24hr time" $ do
            zst <- getzst
            testTime zst "tomorrow 0000" "2016-02-27 08:00:00 UTC"
            testTime zst "tomorrow 0100" "2016-02-27 09:00:00 UTC"
            testTime zst "tomorrow 1100" "2016-02-27 19:00:00 UTC"
            testTime zst "tomorrow 1159" "2016-02-27 19:59:00 UTC"
            testTime zst "tomorrow 1200" "2016-02-27 20:00:00 UTC"
            testTime zst "tomorrow 1559" "2016-02-27 23:59:00 UTC"
            testTime zst "tomorrow 1600" "2016-02-28 00:00:00 UTC"
            testTime zst "tomorrow 1700" "2016-02-28 01:00:00 UTC"
    describe "time with absolute date" $ do
        it "handles summer time" $ do
            zst <- getzst
            testTime zst "0701 0000" "2016-07-01 07:00:00 UTC"
            testTime zst "0701 0100" "2016-07-01 08:00:00 UTC"
            testTime zst "0701 1100" "2016-07-01 18:00:00 UTC"
            testTime zst "0701 1159" "2016-07-01 18:59:00 UTC"
            testTime zst "0701 1200" "2016-07-01 19:00:00 UTC"
            testTime zst "0701 1559" "2016-07-01 22:59:00 UTC"
            testTime zst "0701 1600" "2016-07-01 23:00:00 UTC"
            testTime zst "0701 1659" "2016-07-01 23:59:00 UTC"
            testTime zst "0701 1700" "2016-07-02 00:00:00 UTC"
            testTime zst "0701 1800" "2016-07-02 01:00:00 UTC"


{--
Daylight Saving Time (United States) 2016 begins at 2:00 AM on
Sunday, March 13
and ends at 2:00 AM on
Sunday, November 6
--}
        it "handles pst to pdt" $ do
            zst <- getzst
            testTime zst "03/13 1am" "2016-03-13 09:00:00 UTC"
            testTime zst "03/13 1:59am" "2016-03-13 09:59:00 UTC"
            -- spring forward, there is no 2am
            testTime zst "03/13 2am" "" `shouldThrow` anyException
            testTime zst "03/13 2:59am" "" `shouldThrow` anyException
            testTime zst "03/13 3am" "2016-03-13 10:00:00 UTC"
            testTime zst "03/13 3:59am" "2016-03-13 10:59:00 UTC"
            testTime zst "03/14 1am" "2016-03-14 08:00:00 UTC"
        it "handles pdt to pst" $ do
            zst <- getzst
            testTime zst "11/05 1am" "2016-11-05 08:00:00 UTC"
            testTime zst "11/06 12am" "2016-11-06 07:00:00 UTC"
            testTime zst "11/06 12:59am" "2016-11-06 07:59:00 UTC"
            -- fall back, 2am -> 1am
            testTime zst "11/06 1am" "2016-11-06 09:00:00 UTC"
            testTime zst "11/06 1:59am" "2016-11-06 09:59:00 UTC"
            testTime zst "11/06 2am" "2016-11-06 10:00:00 UTC"
            testTime zst "11/06 2:59am" "2016-11-06 10:59:00 UTC"
        it "handles utc entry" $ do
            zst <- getzst
            testTime zst "130000z" "2016-03-13 00:00:00 UTC"
            testTime zst "131234z" "2016-03-13 12:34:00 UTC"
            testTime zst "011234z" "2016-03-01 12:34:00 UTC"
            testTime zst "281234z" "2016-02-28 12:34:00 UTC"
            testTime zst "291234z" "2016-02-29 12:34:00 UTC" -- leap yr
            testTime zst "301234z" "" `shouldThrow` anyException

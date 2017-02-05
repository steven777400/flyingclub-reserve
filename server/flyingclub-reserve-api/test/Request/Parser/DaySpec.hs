{-# LANGUAGE OverloadedStrings #-}
module Request.Parser.DaySpec where

import           Data.Attoparsec.Text

import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate
import           Data.Time.Clock
import           Data.Time.LocalTime.TimeZone.Olson
import           Data.Time.LocalTime.TimeZone.Series


import           Request.Parser.Day
import           Test.Hspec

-- 57445 = a saturday, 2/27/16, by utc
originDay = ModifiedJulianDay 57445
utcTestDay = UTCTime originDay 0
{--
withToday f = do
    tzs <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/America/Los_Angeles"
    let today = ZoneSeriesTime utcTestDay tzs
    f today
--}

testDay str add = do
    let day = parseOnly (dayFromToday <* endOfInput) str
    case day of
        Left err -> error err
        Right f  -> f originDay
    `shouldBe` (addDays add originDay)

testDay' str dayv = do
    let day = parseOnly (dayFromToday <* endOfInput) str
    case day of
        Left err -> error err
        Right f  -> f originDay
    `shouldBe` dayv

spec :: Spec
spec = do
    describe "dayFromToday" $ do
        it "identify today" $ testDay "today" 0
        it "identify tomorrow" $ testDay "tomorrow" 1
        it "identify tomorrow" $ testDay "tom" 1
        it "identify monday" $ testDay "monday" 2 -- Sat -> Mon
        it "identify saturday" $ testDay "saturday" 0 -- Sat is today
        it "identify sunday" $ testDay "sunday" 1 -- Sat is today
        it "identify friday" $ testDay "friday" 6 -- week minus day
    describe "dayFromExplicit with /" $ do
        it "handles today" $ testDay "2/27" 0
        it "handles tomorrow" $ testDay "2/28" 1
        it "handles leap year" $ testDay "2/29" 2
        it "handles next month" $ testDay "3/1" 3
        it "handles late next month" $ testDay "3/20" 22
        it "handles later year" $ testDay' "11/7" (fromGregorian 2016 11 7)
        it "handles next year md" $ testDay' "1/5" (fromGregorian 2017 1 5)
        it "handles next year mdd" $ testDay' "1/10" (fromGregorian 2017 1 10)
        it "handles next year mmd" $ testDay' "02/5" (fromGregorian 2017 2 5)
        it "handles next year mmdd" $ testDay' "02/15" (fromGregorian 2017 2 15)
    describe "dayFromExplicit without /" $ do
        it "handles today" $ testDay "0227" 0
        it "handles tomorrow" $ testDay "0228" 1
        it "handles leap year" $ testDay "0229" 2
        it "handles next month" $ testDay "0301" 3
        it "handles late next month" $ testDay "0320" 22
        it "handles later year" $ testDay' "1107" (fromGregorian 2016 11 7)
        it "handles next year md" $ testDay' "0105" (fromGregorian 2017 1 5)
        it "handles next year mdd" $ testDay' "0110" (fromGregorian 2017 1 10)
        it "handles next year mmd" $ testDay' "0205" (fromGregorian 2017 2 5)
        it "handles next year mmdd" $ testDay' "0215" (fromGregorian 2017 2 15)

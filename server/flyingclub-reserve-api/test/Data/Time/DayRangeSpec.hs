{-# LANGUAGE OverloadedStrings #-}
module Data.Time.DayRangeSpec where

import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.LocalTime.TimeZone.Olson
import           Data.Time.LocalTime.TimeZone.Series

import           Data.Time.DayRange
import           Test.Hspec

-- 57445 = a saturday, 2/27/16, by utc
originDay = ModifiedJulianDay 57445
{--
Daylight Saving Time (United States) 2016 begins at 2:00 AM on
Sunday, March 13
and ends at 2:00 AM on
Sunday, November 6
--}
summerDay = addDays 60 originDay
pstToPdtDay = fromGregorian 2016 3 13
pdtToPstDay = fromGregorian 2016 11 6

getzst = do
    tzs <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/America/Los_Angeles"
    return $ ZoneSeriesTime (UTCTime originDay 0) tzs


spec :: Spec
spec = do
    describe "dayRange" $ do
        -- so getzst is 2/26/16 4pm
        -- so getzst' is local 2/27/16 4am
        it "handles PST day" $ do
          zst <- getzst
          -- 8 hour difference during standard time
          dayRange (zoneSeriesTimeSeries zst) originDay `shouldBe` (UTCTime originDay (8*60*60), UTCTime (addDays 1 originDay) (8*60*60 - 1))
        it "handles PDT day" $ do
          zst <- getzst
          -- 7 hour difference during saving time
          dayRange (zoneSeriesTimeSeries zst) summerDay `shouldBe` (UTCTime summerDay (7*60*60), UTCTime (addDays 1 summerDay) (7*60*60 - 1))
        it "handles PST -> PDT day" $ do
          zst <- getzst
          -- 8 -> 7 hour difference during spring forward
          dayRange (zoneSeriesTimeSeries zst) pstToPdtDay `shouldBe` (UTCTime pstToPdtDay (8*60*60), UTCTime (addDays 1 pstToPdtDay) (7*60*60 - 1))
        it "handles PDT -> PST day" $ do
          zst <- getzst
          -- 7 -> 8 hour difference during fall back
          dayRange (zoneSeriesTimeSeries zst) pdtToPstDay `shouldBe` (UTCTime pdtToPstDay (7*60*60), UTCTime (addDays 1 pdtToPstDay) (8*60*60 - 1))
    describe "enumerateLocalHours" $ do
        it "handles PST day" $ do
          zst <- getzst
          -- 8 hour difference during standard time
          let x = enumerateLocalHours (zoneSeriesTimeSeries zst) originDay
          slotUtc (head x) `shouldBe` UTCTime originDay (8*60*60)
          slotUtc (last x) `shouldBe` UTCTime (addDays 1 originDay) (7*60*60)
          slotLocal (head x) `shouldBe` LocalTime originDay midnight
          slotLocal (last x) `shouldBe` LocalTime originDay (TimeOfDay 23 0 0)
          slotDisplay (head x) `shouldBe` "midnight"
          slotDisplay (last x) `shouldBe` "11:00 PM"
          length x `shouldBe` 24

        it "handles PDT day" $ do
          zst <- getzst
          -- 7 hour difference during saving time
          let x = enumerateLocalHours (zoneSeriesTimeSeries zst) summerDay
          slotUtc (head x) `shouldBe` UTCTime summerDay (7*60*60)
          slotUtc (last x) `shouldBe` UTCTime (addDays 1 summerDay) (6*60*60)
          slotLocal (head x) `shouldBe` LocalTime summerDay midnight
          slotLocal (last x) `shouldBe` LocalTime summerDay (TimeOfDay 23 0 0)
          slotDisplay (head x) `shouldBe` "midnight"
          slotDisplay (last x) `shouldBe` "11:00 PM"
          length x `shouldBe` 24
        it "handles PST -> PDT day" $ do
          zst <- getzst
          -- 8 -> 7 hour difference during spring forward
          let x = enumerateLocalHours (zoneSeriesTimeSeries zst) pstToPdtDay
          slotUtc (head x) `shouldBe` UTCTime pstToPdtDay (8*60*60)
          slotUtc (last x) `shouldBe` UTCTime (addDays 1 pstToPdtDay) (6*60*60)
          slotLocal (head x) `shouldBe` LocalTime pstToPdtDay midnight
          slotLocal (last x) `shouldBe` LocalTime pstToPdtDay (TimeOfDay 23 0 0)
          slotDisplay (head x) `shouldBe` "midnight"
          slotDisplay (last x) `shouldBe` "11:00 PM"
          length x `shouldBe` 23
        it "handles PDT -> PST day" $ do
          zst <- getzst
          -- 7 -> 8 hour difference during fall back
          let x = enumerateLocalHours (zoneSeriesTimeSeries zst) pdtToPstDay
          slotUtc (head x) `shouldBe` UTCTime pdtToPstDay (7*60*60)
          slotUtc (last x) `shouldBe` UTCTime (addDays 1 pdtToPstDay) (7*60*60)
          slotLocal (head x) `shouldBe` LocalTime pdtToPstDay midnight
          slotLocal (last x) `shouldBe` LocalTime pdtToPstDay (TimeOfDay 23 0 0)
          slotDisplay (head x) `shouldBe` "midnight"
          slotDisplay (last x) `shouldBe` "11:00 PM"
          length x `shouldBe` 25

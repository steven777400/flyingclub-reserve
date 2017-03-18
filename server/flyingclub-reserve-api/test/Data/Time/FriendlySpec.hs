{-# LANGUAGE OverloadedStrings #-}
module Data.Time.FriendlySpec where

import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.LocalTime.TimeZone.Olson
import           Data.Time.LocalTime.TimeZone.Series

import           Data.Time.Friendly
import           Test.Hspec

-- 57445 = a saturday, 2/27/16, by utc
originDay = ModifiedJulianDay 57445
utcOrigin = UTCTime originDay 0
-- so utcOrigin is 2/26/16 4pm
utcOrigin' = UTCTime originDay (12*60*60)
-- so utcOrigin' is local 2/27/16 4am


getzst = do
    tzs <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/America/Los_Angeles"
    return $ ZoneSeriesTime utcOrigin tzs

getzst' = do
    tzs <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/America/Los_Angeles"
    return $ ZoneSeriesTime utcOrigin' tzs

spec :: Spec
spec = do
    describe "formatDay" $ do
        it "identify today" $ formatDay originDay originDay `shouldBe` "today"
        it "identify tomorrow" $ formatDay originDay (addDays 1 originDay) `shouldBe` "tomorrow"
        it "identify monday" $ formatDay originDay (addDays 2 originDay) `shouldBe` "Monday, February 29th"
        it "identify Tuesday" $ formatDay originDay (addDays 3 originDay) `shouldBe` "Tuesday, March 1st"
        it "identify Wednesday" $ formatDay originDay (addDays 4 originDay) `shouldBe` "Wednesday, March 2nd"
        it "identify Thursday" $ formatDay originDay (addDays 5 originDay) `shouldBe` "Thursday, March 3rd"
        it "identify Friday" $ formatDay originDay (addDays 6 originDay) `shouldBe` "Friday, March 4th"
        it "identify Saturday" $ formatDay originDay (addDays 7 originDay) `shouldBe` "Saturday, March 5th"
        it "identify Sunday" $ formatDay originDay (addDays 8 originDay) `shouldBe` "Sunday, March 6th"
        it "identify ordinal multi-digit 11th" $ formatDay originDay (addDays 13 originDay) `shouldBe` "Friday, March 11th"
        it "identify ordinal multi-digit 21st" $ formatDay originDay (addDays 23 originDay) `shouldBe` "Monday, March 21st"
        it "identify future month" $ formatDay originDay (addDays 300 originDay) `shouldBe` "Friday, December 23rd"
        it "identify future year" $ formatDay originDay (addDays 320 originDay) `shouldBe` "Thursday, January 12th, 2017"
    describe "formatTime" $ do
        it "handles midnight" $ formatTime midnight `shouldBe` "12:00 AM"
        it "handles noon" $ formatTime midday `shouldBe` "12:00 PM"
        it "handles 0005" $ formatTime (TimeOfDay 0 5 0) `shouldBe` "12:05 AM"
        it "handles 0015" $ formatTime (TimeOfDay 0 15 0) `shouldBe` "12:15 AM"
        it "handles 0900" $ formatTime (TimeOfDay 9 0 0) `shouldBe` "9:00 AM"
        it "handles 0905" $ formatTime (TimeOfDay 9 05 0) `shouldBe` "9:05 AM"
        it "handles 1205" $ formatTime (TimeOfDay 12 05 0) `shouldBe` "12:05 PM"
        it "handles 1345" $ formatTime (TimeOfDay 13 45 0) `shouldBe` "1:45 PM"
        it "handles 2359" $ formatTime (TimeOfDay 23 59 0) `shouldBe` "11:59 PM"
    describe "formatZSTUTC" $ do
        -- so getzst is 2/26/16 4pm
        -- so getzst' is local 2/27/16 4am
        it "handles same day" $ do
          zst <- getzst
          formatZSTUTC zst utcOrigin `shouldBe` "today 4:00 PM"
          zst' <- getzst'
          formatZSTUTC zst' utcOrigin' `shouldBe` "today 4:00 AM"
        it "handles next day" $ do
          zst <- getzst
          formatZSTUTC zst utcOrigin' `shouldBe` "tomorrow 4:00 AM"
        it "handles other day" $ do
          zst' <- getzst'
          formatZSTUTC zst' utcOrigin `shouldBe` "Friday, February 26th 4:00 PM"
    describe "formatZSTUTCPair" $ do
        -- so getzst is 2/26/16 4pm
        -- so getzst' is local 2/27/16 4am
        it "handles different local days" $ do
          zst <- getzst
          formatZSTUTCPair zst utcOrigin utcOrigin' `shouldBe` "today 4:00 PM until tomorrow 4:00 AM"
          formatZSTUTCPair zst utcOrigin' (UTCTime (addDays 3 originDay) 0) `shouldBe` "tomorrow 4:00 AM until Monday, February 29th 4:00 PM"
        it "handles same local days" $ do
          zst <- getzst
          formatZSTUTCPair zst utcOrigin (UTCTime originDay (60*60)) `shouldBe` "today 4:00 PM until 5:00 PM"
          formatZSTUTCPair zst utcOrigin' (UTCTime originDay (13*60*60)) `shouldBe` "tomorrow 4:00 AM until 5:00 AM"

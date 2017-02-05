{-# LANGUAGE OverloadedStrings #-}
module Data.Time.FriendlySpec where

import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.Clock

import Test.Hspec
import Data.Time.Friendly

-- 57445 = a saturday, 2/27/16, by utc
originDay = ModifiedJulianDay 57445


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

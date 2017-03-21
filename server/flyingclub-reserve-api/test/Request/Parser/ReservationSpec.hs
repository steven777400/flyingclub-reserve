{-# LANGUAGE OverloadedStrings #-}
module Request.Parser.ReservationSpec where

import           Data.Attoparsec.Text
import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate
import           Data.Time.Clock
import           Data.Time.LocalTime.TimeZone.Olson
import           Data.Time.LocalTime.TimeZone.Series

import           Data.ParsedAction
import           Request.Parser.Reservation
import           Test.Hspec

instance Show ParsedAction where
  
-- 57445 = a saturday, 2/27/16, by utc
originDay = ModifiedJulianDay 57445
utcOrigin = UTCTime originDay (12*60*60)
-- so utcOrigin is local 2/27/16 4am

getzst = do
    tzs <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/America/Los_Angeles"
    return $ ZoneSeriesTime utcOrigin tzs

testTime zst str result = do
    let day = parseOnly (actionFromText <* endOfInput) str
    case day of
        Left err -> error err
        Right f  -> f zst
    `shouldBe` result


spec :: Spec
spec = do
  describe "reserve" $ do
      it "reserve for time range" $ do
        zst <- getzst
        testTime zst "reserve 073 11am 4pm" $ Reserve "073" (addUTCTime (7*60*60) utcOrigin) (addUTCTime (12*60*60) utcOrigin)
        testTime zst "reserve 073 11am today 4pm" $ Reserve "073" (addUTCTime (7*60*60) utcOrigin) (addUTCTime (12*60*60) utcOrigin)
        testTime zst "reserve 073 today 11am 4pm" $ Reserve "073" (addUTCTime (7*60*60) utcOrigin) (addUTCTime (12*60*60) utcOrigin)
        testTime zst "reserve 073 tomorrow 11am 4pm" $ Reserve "073" (addUTCTime ((24+7)*60*60) utcOrigin) (addUTCTime ((24+12)*60*60) utcOrigin)
        testTime zst "reserve 073 today 11am tomorrow 4pm" $ Reserve "073" (addUTCTime (7*60*60) utcOrigin) (addUTCTime ((24+12)*60*60) utcOrigin)
        testTime zst "reserve 074 11am 2/27 4pm" $ Reserve "074" (addUTCTime (7*60*60) utcOrigin) (addUTCTime (12*60*60) utcOrigin)
        testTime zst "reserve 075 2/27 11am 2/27 4pm" $ Reserve "075" (addUTCTime (7*60*60) utcOrigin) (addUTCTime (12*60*60) utcOrigin)
        testTime zst "reserve 075 2/27 11am 2/28 4pm" $ Reserve "075" (addUTCTime (7*60*60) utcOrigin) (addUTCTime ((24+12)*60*60) utcOrigin)
        testTime zst "reserve 075 2/27 11am 4pm" $ Reserve "075" (addUTCTime (7*60*60) utcOrigin) (addUTCTime (12*60*60) utcOrigin)
        testTime zst "reserve 076 11am tomorrow 4pm" $ Reserve "076" (addUTCTime (7*60*60) utcOrigin) (addUTCTime ((24+12)*60*60) utcOrigin)
        testTime zst "reserve 077 tomorrow 4pm 3/1 11am" $ Reserve "077" (addUTCTime (36*60*60) utcOrigin) (addUTCTime ((7+72)*60*60) utcOrigin)
        testTime zst "Reserve 078 0227 1100 0227 1600" $ Reserve "078" (addUTCTime (7*60*60) utcOrigin) (addUTCTime (12*60*60) utcOrigin)
        testTime zst "Reserve 078 0227 1100 1600" $ Reserve "078" (addUTCTime (7*60*60) utcOrigin) (addUTCTime (12*60*60) utcOrigin)
        testTime zst "RESERVE 079 Tuesday 7am Thursday 5pm" $ Reserve "079" (addUTCTime ((3+72)*60*60) utcOrigin) (addUTCTime ((13+120)*60*60) utcOrigin)
        testTime zst "RESERVE 079 Tuesday 7am 5pm" $ Reserve "079" (addUTCTime ((3+72)*60*60) utcOrigin) (addUTCTime ((13+72)*60*60) utcOrigin)
        testTime zst "Reserve 080 271300Z 271600Z" $ Reserve "080" (addUTCTime (1*60*60) utcOrigin) (addUTCTime (4*60*60) utcOrigin)
        testTime zst "Reserve 081 281300Z 281600Z" $ Reserve "081" (addUTCTime (25*60*60) utcOrigin) (addUTCTime (28*60*60) utcOrigin)
        testTime zst "Reserve 081 281300Z 011200Z" $ Reserve "081" (addUTCTime (25*60*60) utcOrigin) (addUTCTime (72*60*60) utcOrigin)
      it "reserve for time range with extra word" $ do
        zst <- getzst
        testTime zst "reserve 073 11am to 4pm" $ Reserve "073" (addUTCTime (7*60*60) utcOrigin) (addUTCTime (12*60*60) utcOrigin)
        testTime zst "reserve 073 today 11am to 4pm" $ Reserve "073" (addUTCTime (7*60*60) utcOrigin) (addUTCTime (12*60*60) utcOrigin)
        testTime zst "reserve 073 11am to today 4pm" $ Reserve "073" (addUTCTime (7*60*60) utcOrigin) (addUTCTime (12*60*60) utcOrigin)
        testTime zst "reserve 074 11am until 2/27 4pm" $ Reserve "074" (addUTCTime (7*60*60) utcOrigin) (addUTCTime (12*60*60) utcOrigin)
        testTime zst "reserve 075 2/27 11am til 2/27 4pm" $ Reserve "075" (addUTCTime (7*60*60) utcOrigin) (addUTCTime (12*60*60) utcOrigin)
        testTime zst "reserve 076 11am till tomorrow 4pm" $ Reserve "076" (addUTCTime (7*60*60) utcOrigin) (addUTCTime ((24+12)*60*60) utcOrigin)
        testTime zst "reserve 077 tomorrow 4pm to 3/1 11am" $ Reserve "077" (addUTCTime (36*60*60) utcOrigin) (addUTCTime ((7+72)*60*60) utcOrigin)
        testTime zst "Reserve 078 0227 1100 until 0227 1600" $ Reserve "078" (addUTCTime (7*60*60) utcOrigin) (addUTCTime (12*60*60) utcOrigin)
        testTime zst "RESERVE 079 Tuesday 7am to Thursday 5pm" $ Reserve "079" (addUTCTime ((3+72)*60*60) utcOrigin) (addUTCTime ((13+120)*60*60) utcOrigin)
  describe "cancel" $ do
      it "cancel for time" $ do
        zst <- getzst
        testTime zst "cancel 073 11am" $ Cancel (Just "073") (addUTCTime (7*60*60) utcOrigin)
        testTime zst "cancel 073 today 11am" $ Cancel (Just "073") (addUTCTime (7*60*60) utcOrigin)
        testTime zst "cancel 075 2/27 11am" $ Cancel (Just "075") (addUTCTime (7*60*60) utcOrigin)
        testTime zst "Cancel 077 tomorrow 4pm" $ Cancel (Just "077") (addUTCTime (36*60*60) utcOrigin)
        testTime zst "CANCEL 078 0227 1100" $ Cancel (Just "078") (addUTCTime (7*60*60) utcOrigin)
      it "quick cancel" $ do
        zst <- getzst
        testTime zst "cancel" $ Cancel Nothing utcOrigin

  describe "check" $ do
      it "defaults to today" $ do
        zst <- getzst
        testTime zst "check 073" $ Check "073" originDay
      it "accepts relative date" $ do
        zst <- getzst
        testTime zst "Check N54073 today" $ Check "N54073" originDay
        testTime zst "Check N54073 tomorrow" $ Check "N54073" (addDays 1 originDay)
      it "accepts date" $ do
        zst <- getzst
        testTime zst "Check N54073 Monday" $ Check "N54073" (addDays 2 originDay)
        testTime zst "Check N54073 3/1" $ Check "N54073" (addDays 3 originDay)
  describe "review" $ do
      it "defaults to today" $ do
        zst <- getzst
        testTime zst "review" $ Review originDay
      it "accepts relative date" $ do
        zst <- getzst
        testTime zst "Review today" $ Review originDay
        testTime zst "Review tomorrow" $ Review (addDays 1 originDay)
      it "accepts date" $ do
        zst <- getzst
        testTime zst "Review Monday" $ Review (addDays 2 originDay)
        testTime zst "Review 3/1" $ Review (addDays 3 originDay)
  describe "update" $ do
      it "update or extend for time" $ do
        zst <- getzst
        testTime zst "update 11am" $ Update (addUTCTime (7*60*60) utcOrigin)
        testTime zst "extend today 11am" $ Update (addUTCTime (7*60*60) utcOrigin)
        testTime zst "update 2/27 11am" $ Update (addUTCTime (7*60*60) utcOrigin)
        testTime zst "Extend tomorrow 4pm" $ Update (addUTCTime (36*60*60) utcOrigin)
        testTime zst "UPDATE 0227 1100" $ Update (addUTCTime (7*60*60) utcOrigin)

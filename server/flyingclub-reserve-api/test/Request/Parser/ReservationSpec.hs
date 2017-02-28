{-# LANGUAGE OverloadedStrings #-}
module Request.Parser.ReservationSpec where

import           Data.Attoparsec.Text
import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate
import           Data.Time.Clock

import           Data.ParsedAction
import           Request.Parser.Reservation
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

testParse parser str result = do
  let res = parseOnly (parser <* endOfInput) str
  case res of
      Left err -> error err
      Right f  -> f originDay
  `shouldBe` result


spec :: Spec
spec = do
    describe "check" $ do
        it "defaults to today" $ testParse check "check 073" $ Check "073" originDay
        it "accepts date" $ testParse check "Check N54073 Monday" $ Check "N54073" (addDays 2 originDay)

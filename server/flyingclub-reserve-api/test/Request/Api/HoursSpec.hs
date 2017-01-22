{-# LANGUAGE OverloadedStrings #-}
module Request.Api.HoursSpec where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime.TimeZone.Series
import Data.Time.LocalTime.TimeZone.Olson


import Test.Hspec
import Request.Api.Hours


gettzs = getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/America/Los_Angeles"

hoursShouldBe (begin, end) hours =
    let mins = round $ (diffUTCTime end begin) / 60 -- round due to leap seconds, lol
    in
    mins `shouldBe` hours*60

spec :: Spec
spec = do
    describe "localDayTimeRange" $ do
        it "identify winter range" $ do
            tzs <- gettzs
            let (begin, end) = localDayTimeRange tzs (fromGregorian 2016 01 20)
            begin `shouldBe` (UTCTime (fromGregorian 2016 01 20) (8*60*60))
            end `shouldBe` (UTCTime (fromGregorian 2016 01 21) (8*60*60 - 1))
            (begin, end) `hoursShouldBe` 24            
        it "identify summer range" $ do
            tzs <- gettzs
            let (begin, end) = localDayTimeRange tzs (fromGregorian 2016 08 20)
            begin `shouldBe` (UTCTime (fromGregorian 2016 08 20) (7*60*60))
            end `shouldBe` (UTCTime (fromGregorian 2016 08 21) (7*60*60 - 1)) 
            (begin, end) `hoursShouldBe` 24            
        it "cross month border" $ do
            tzs <- gettzs
            let (begin, end) = localDayTimeRange tzs (fromGregorian 2016 08 31)
            begin `shouldBe` (UTCTime (fromGregorian 2016 08 31) (7*60*60))
            end `shouldBe` (UTCTime (fromGregorian 2016 09 1) (7*60*60 - 1))
            (begin, end) `hoursShouldBe` 24            
        it "cross year border" $ do
            tzs <- gettzs
            let (begin, end) = localDayTimeRange tzs (fromGregorian 2016 12 31)
            begin `shouldBe` (UTCTime (fromGregorian 2016 12 31) (8*60*60))
            end `shouldBe` (UTCTime (fromGregorian 2017 01 1) (8*60*60 - 1))
            (begin, end) `hoursShouldBe` 24
{--
Daylight Saving Time (United States) 2016 begins at 2:00 AM on
Sunday, March 13
and ends at 2:00 AM on
Sunday, November 6
--}            
        it "handles pst to pdt" $ do
            tzs <- gettzs
            let (begin, end) = localDayTimeRange tzs (fromGregorian 2016 3 13)
            begin `shouldBe` (UTCTime (fromGregorian 2016 3 13) (8*60*60))
            end `shouldBe` (UTCTime (fromGregorian 2016 3 14) (7*60*60 - 1))
            (begin, end) `hoursShouldBe` 23 -- spring forward, one hour lost            
        it "handles pdt to pst" $ do
            tzs <- gettzs
            let (begin, end) = localDayTimeRange tzs (fromGregorian 2016 11 6)
            begin `shouldBe` (UTCTime (fromGregorian 2016 11 6) (7*60*60))
            end `shouldBe` (UTCTime (fromGregorian 2016 11 7) (8*60*60 - 1))
            (begin, end) `hoursShouldBe` 25 -- fall back, one extra hour
            
    describe "localDayHours" $ do
        it "handles winter day" $ do
            tzs <- gettzs
            let hrs = localDayHours tzs (fromGregorian 2016 01 20)
            length hrs `shouldBe` 24
            let (LocalHour fhr fpm futc) = head hrs
            fhr `shouldBe` 12
            fpm `shouldBe` False
            futc `shouldBe` UTCTime (fromGregorian 2016 01 20) (8*60*60)
            let (LocalHour shr spm sutc) = hrs !! 1
            shr `shouldBe` 1
            spm `shouldBe` False
            sutc `shouldBe` UTCTime (fromGregorian 2016 01 20) (9*60*60)
            let (LocalHour nhr npm nutc) = hrs !! 12
            nhr `shouldBe` 12
            npm `shouldBe` True
            nutc `shouldBe` UTCTime (fromGregorian 2016 01 20) (20*60*60)
            let (LocalHour ehr epm eutc) = last hrs
            ehr `shouldBe` 11
            epm `shouldBe` True
            eutc `shouldBe` UTCTime (fromGregorian 2016 01 21) (7*60*60)
            
        it "handles summer day" $ do
            tzs <- gettzs
            let hrs = localDayHours tzs (fromGregorian 2016 08 20)
            length hrs `shouldBe` 24
            let (LocalHour fhr fpm futc) = head hrs
            fhr `shouldBe` 12
            fpm `shouldBe` False
            futc `shouldBe` UTCTime (fromGregorian 2016 08 20) (7*60*60)
            let (LocalHour shr spm sutc) = hrs !! 1
            shr `shouldBe` 1
            spm `shouldBe` False
            sutc `shouldBe` UTCTime (fromGregorian 2016 08 20) (8*60*60)
            let (LocalHour nhr npm nutc) = hrs !! 12
            nhr `shouldBe` 12
            npm `shouldBe` True
            nutc `shouldBe` UTCTime (fromGregorian 2016 08 20) (19*60*60)
            let (LocalHour ehr epm eutc) = last hrs
            ehr `shouldBe` 11
            epm `shouldBe` True
            eutc `shouldBe` UTCTime (fromGregorian 2016 08 21) (6*60*60)   
            
        it "handles pst to pdt" $ do
            tzs <- gettzs            
            let hrs = localDayHours tzs (fromGregorian 2016 3 13)
            length hrs `shouldBe` 23 -- spring forward, skip an hour
            let (LocalHour fhr fpm futc) = head hrs
            fhr `shouldBe` 12
            fpm `shouldBe` False
            futc `shouldBe` UTCTime (fromGregorian 2016 3 13) (8*60*60)
            let (LocalHour shr spm sutc) = hrs !! 1
            shr `shouldBe` 1
            spm `shouldBe` False
            sutc `shouldBe` UTCTime (fromGregorian 2016 3 13) (9*60*60)
            let (LocalHour thr tpm tutc) = hrs !! 2 -- spring forward, 3am
            thr `shouldBe` 3
            tpm `shouldBe` False
            tutc `shouldBe` UTCTime (fromGregorian 2016 3 13) (10*60*60)
            let (LocalHour nhr npm nutc) = hrs !! 11
            nhr `shouldBe` 12
            npm `shouldBe` True
            nutc `shouldBe` UTCTime (fromGregorian 2016 3 13) (19*60*60)
            let (LocalHour ehr epm eutc) = last hrs
            ehr `shouldBe` 11
            epm `shouldBe` True
            eutc `shouldBe` UTCTime (fromGregorian 2016 3 14) (6*60*60)            

        it "handles pdt to pst" $ do
            tzs <- gettzs            
            let hrs = localDayHours tzs (fromGregorian 2016 11 6)
            length hrs `shouldBe` 25 -- fall back, add an hour
            let (LocalHour fhr fpm futc) = head hrs
            fhr `shouldBe` 12
            fpm `shouldBe` False
            futc `shouldBe` UTCTime (fromGregorian 2016 11 6) (7*60*60)
            let (LocalHour shr spm sutc) = hrs !! 1
            shr `shouldBe` 1
            spm `shouldBe` False
            sutc `shouldBe` UTCTime (fromGregorian 2016 11 6) (8*60*60)
            let (LocalHour thr tpm tutc) = hrs !! 2 -- fall back, 1am
            thr `shouldBe` 1
            tpm `shouldBe` False
            tutc `shouldBe` UTCTime (fromGregorian 2016 11 6) (9*60*60)
            let (LocalHour nhr npm nutc) = hrs !! 13
            nhr `shouldBe` 12
            npm `shouldBe` True
            nutc `shouldBe` UTCTime (fromGregorian 2016 11 6) (20*60*60)
            let (LocalHour ehr epm eutc) = last hrs
            ehr `shouldBe` 11
            epm `shouldBe` True
            eutc `shouldBe` UTCTime (fromGregorian 2016 11 7) (7*60*60)             

    describe "localSunriseSunset" $ do
        it "handles winter day" $ do
            tzs <- gettzs
            let (LocalSunriseSunset rise set) = localSunriseSunset (122.91, 47.01) tzs (fromGregorian 2016 01 20)            
            let (LocalHour risehour risepm _, LocalHour sethour setpm _) = (rise, set)
            risehour `shouldBe` 7
            risepm `shouldBe` False
            sethour `shouldBe` 4
            setpm `shouldBe` True 
        it "handles pst to pdt day" $ do
            tzs <- gettzs
            let (LocalSunriseSunset rise set) = localSunriseSunset (122.91, 47.01) tzs (fromGregorian 2016 03 13)            
            let (LocalHour risehour risepm _, LocalHour sethour setpm _) = (rise, set)            
            risehour `shouldBe` 7
            risepm `shouldBe` False
            sethour `shouldBe` 7
            setpm `shouldBe` True
        it "handles summer day" $ do
            tzs <- gettzs
            let (LocalSunriseSunset rise set) = localSunriseSunset (122.91, 47.01) tzs (fromGregorian 2016 6 22)            
            let (LocalHour risehour risepm _, LocalHour sethour setpm _) = (rise, set)            
            risehour `shouldBe` 5
            risepm `shouldBe` False
            sethour `shouldBe` 9
            setpm `shouldBe` True
        it "handles pdt to pst day" $ do
            tzs <- gettzs
            let (LocalSunriseSunset rise set) = localSunriseSunset (122.91, 47.01) tzs (fromGregorian 2016 11 6)            
            let (LocalHour risehour risepm _, LocalHour sethour setpm _) = (rise, set)            
            risehour `shouldBe` 7
            risepm `shouldBe` False
            sethour `shouldBe` 4
            setpm `shouldBe` True            
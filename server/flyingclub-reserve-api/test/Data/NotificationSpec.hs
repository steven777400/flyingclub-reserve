{-# LANGUAGE OverloadedStrings #-}
module Data.NotificationSpec where


import           Control.Exception.Conflict
import           Control.Exception.Unauthorized
import           Control.Monad.Catch
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Either
import           Data.Time.Calendar
import           Data.Time.Clock
import qualified Database.Persist.Audit.Operations  as A
import           Database.Persist.Notification
import qualified Database.Persist.Schema            as S
import           Database.Persist.Sqlite
import           Database.Persist.Types.PhoneNumber
import           Database.Persist.Types.UserType
import           Database.Persist.Types.UUID
import           Request.Api.AuthorizedAction
import           System.Random
import           Data.Time.LocalTime.TimeZone.Olson
import           Data.Time.LocalTime.TimeZone.Series
import Control.Monad.Reader

import           Request.Api.Reservation
import           Request.Api.User
import           Request.Api.Airplane
import           Request.Api.ParsedAction
import Data.ParsedAction
import Data.ParsedActionResult
import Data.Notification
import           Test.Hspec


anyUnauthorizedException :: Selector UnauthorizedException
anyUnauthorizedException = const True

anyConflictException :: Selector ConflictException
anyConflictException = const True

data SampleData = SampleData {
  officerUser :: Key S.User,
  pilotUser   :: Key S.User,
  socialUser  :: Key S.User,
  naUser      :: Key S.User,
  n073        :: Key S.Airplane,
  n349        :: Key S.Airplane,
  n666        :: Key S.Airplane,
  pilotRes    :: Key S.Reservation,
  officerRes  :: Key S.Reservation

}

runInDb :: (SampleData -> SqlPersistM a) -> IO a
runInDb sql = runSqlite ":memory:" $ do
    S.runAdjustedMigration
    i1 <- liftIO $ S.UserKey <$> (randomIO :: IO UUID)
    i2 <- liftIO $ S.UserKey <$> (randomIO :: IO UUID)
    i3 <- liftIO $ S.UserKey <$> (randomIO :: IO UUID)
    i4 <- liftIO $ S.UserKey <$> (randomIO :: IO UUID)
    ia1 <- liftIO $ S.AirplaneKey <$> (randomIO :: IO UUID)
    ia2 <- liftIO $ S.AirplaneKey <$> (randomIO :: IO UUID)
    ia3 <- liftIO $ S.AirplaneKey <$> (randomIO :: IO UUID)
    insertKey i1 sampleOfficerUser
    insertKey i2 samplePilotUser
    insertKey i3 sampleNAUser
    insertKey i4 sampleSocialUser
    insertKey ia1 a1
    insertKey ia2 a2
    insertKey ia3 a3
    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    let rkr = S.Reservation i2 ia1 (UTCTime (fromGregorian 2016 02 27) (8*60*60)) (UTCTime (fromGregorian 2016 02 27) (10*60*60)) Nothing False ""
    insertKey rk rkr
    let rk1 = rk

    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i1 ia1
      (UTCTime (fromGregorian 2016 02 27) (12*60*60))
      (UTCTime (fromGregorian 2016 02 27) (15*60*60))
      Nothing False ""
    let ork = rk

    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i2 ia1
      (UTCTime (fromGregorian 2016 02 27) (15*60*60))
      (UTCTime (fromGregorian 2016 02 27) (20*60*60))
      Nothing False ""

    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i1 ia1
      (UTCTime (fromGregorian 2016 02 27) (9*60*60))
      (UTCTime (fromGregorian 2016 02 27) (15*60*60))
      (Just $ UTCTime (fromGregorian 2016 02 27) (15*60*60)) False "" -- deleted

    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i1 ia2
      (UTCTime (fromGregorian 2016 02 27) (9*60*60))
      (UTCTime (fromGregorian 2016 02 27) (11*60*60))
      Nothing False ""
    sql (SampleData i1 i2 i4 i3 ia1 ia2 ia3 rk1 ork)

sampleOfficerUser = S.User "testof" "testol" Officer Nothing
samplePilotUser = S.User "testpf" "testpl" Pilot Nothing
sampleSocialUser = S.User "test1f" "test1l" Social Nothing
sampleNAUser = S.User "test1f" "test1l" NoAccess Nothing

a1 = S.Airplane "54073" "cessna 172" Nothing
a2 = S.Airplane "52349" "cessna 182" Nothing
a3 = S.Airplane "666ab" "cessna 666" (Just $ UTCTime (fromGregorian 2010 01 10) 0)

-- 57445 = a saturday, 2/27/16, by utc
-- in this timezone, we expect that 0227 0000 utc -> 2/27 4pm
-- because (4+12) + 8 = 24 -> 00
originDay = ModifiedJulianDay 57445
utcOrigin = UTCTime originDay (12*60*60)
-- so utcOrigin is local 2/26/16 4pm

originDayP = ModifiedJulianDay 57443
utcOriginP = UTCTime originDayP 0

originDay' = ModifiedJulianDay 57445
utcOrigin' = UTCTime originDay' 0

originDayF = ModifiedJulianDay 59445
utcOriginF = UTCTime originDayF 0


getzst origin = do
    tzs <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/America/Los_Angeles"
    return $ ZoneSeriesTime origin tzs

spec :: Spec
spec = do
  describe "parsedActionResultResponse" $ do
        it "check future day with results" $ runInDb $ \sd -> do
            zst <- liftIO $ getzst utcOriginP
            airplanes <- runAuthorizedAction (pilotUser sd) getAirplanes
            users <- runAuthorizedAction (pilotUser sd) getUsers
            r <- runParsedAction zst (pilotUser sd) $ Check "073" originDay
            liftIO $ runReader (parsedActionResultResponse r) (Context SMS zst airplanes users)
              `shouldBe` "The airplane is scheduled by testpf testpl Saturday, February 27th at midnight until 2:00 AM, and by testof testol Saturday, February 27th at 4:00 AM until 7:00 AM, and by testpf testpl Saturday, February 27th at 7:00 AM until noon"
        it "check current day with partial results" $ runInDb $ \sd -> do
            zst <- liftIO $ getzst utcOrigin
            airplanes <- runAuthorizedAction (pilotUser sd) getAirplanes
            users <- runAuthorizedAction (pilotUser sd) getUsers
            r <- runParsedAction zst (pilotUser sd) $ Check "073" originDay
            liftIO $ runReader (parsedActionResultResponse r) (Context SMS zst airplanes users)
              `shouldBe` "The airplane is scheduled by testof testol today at 4:00 AM until 7:00 AM, and by testpf testpl today at 7:00 AM until noon"
        it "check future day with no results" $ runInDb $ \sd -> do
            zst <- liftIO $ getzst utcOrigin
            airplanes <- runAuthorizedAction (pilotUser sd) getAirplanes
            users <- runAuthorizedAction (pilotUser sd) getUsers
            r <- runParsedAction zst (pilotUser sd) $ Check "073" originDayF
            liftIO $ runReader (parsedActionResultResponse r) (Context SMS zst airplanes users)
              `shouldBe` "This airplane is available for the entire day"

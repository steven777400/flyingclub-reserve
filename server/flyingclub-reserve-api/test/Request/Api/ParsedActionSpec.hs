{-# LANGUAGE OverloadedStrings #-}
module Request.Api.ParsedActionSpec where

import           Control.Exception.Conflict
import           Control.Exception.Format
import           Control.Exception.Unauthorized
import           Control.Monad.Catch
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Either
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime.TimeZone.Olson
import           Data.Time.LocalTime.TimeZone.Series
import qualified Database.Persist.Audit.Operations   as A
import           Database.Persist.Environment.Sqlite (runInMemory)
import           Database.Persist.Notification
import qualified Database.Persist.Schema             as S
import           Database.Persist.Sqlite
import           Database.Persist.Types.PhoneNumber
import           Database.Persist.Types.UserType
import           Database.Persist.Types.UUID
import           Request.Api.AuthorizedAction
import           System.Random

import           Data.ParsedAction
import           Data.ParsedActionResult
import           Request.Api.ParsedAction
import           Request.Api.User
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

runInDb :: (SampleData -> S.SqlM a) -> IO a
runInDb sql = runInMemory $ do
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
    let rkr = S.Reservation i2 ia1 (UTCTime (fromGregorian 2016 02 27) (8*60*60)) (UTCTime (fromGregorian 2016 02 27) (10*60*60)) False "" Nothing
    insertKey rk rkr
    let rk1 = rk

    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i1 ia1
      (UTCTime (fromGregorian 2016 02 27) (12*60*60))
      (UTCTime (fromGregorian 2016 02 27) (15*60*60))
      False "" Nothing
    let ork = rk

    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i2 ia1
      (UTCTime (fromGregorian 2016 02 27) (15*60*60))
      (UTCTime (fromGregorian 2016 02 27) (16*60*60))
      False "" Nothing

    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i1 ia1
      (UTCTime (fromGregorian 2016 02 27) (9*60*60))
      (UTCTime (fromGregorian 2016 02 27) (15*60*60))
      False "" (Just $ UTCTime (fromGregorian 2016 02 27) (15*60*60)) -- deleted

    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i1 ia2
      (UTCTime (fromGregorian 2016 02 27) (9*60*60))
      (UTCTime (fromGregorian 2016 02 27) (11*60*60))
      False "" Nothing
    sql (SampleData i1 i2 i4 i3 ia1 ia2 ia3 rk1 ork)

sampleOfficerUser = S.User "test1f" "test1l" Officer (fromGregorian 1990 1 1) Nothing
samplePilotUser = S.User "test1f" "test1l" Pilot (fromGregorian 1990 1 1) Nothing
sampleSocialUser = S.User "test1f" "test1l" Social (fromGregorian 1990 1 1) Nothing
sampleNAUser = S.User "test1f" "test1l" NoAccess (fromGregorian 1990 1 1) Nothing

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
  describe "check" $ do
        it "finds matching reservations" $ runInDb $ \sd -> do
            zst <- liftIO $ getzst utcOrigin
            CheckResult r <- runParsedAction zst (pilotUser sd) $ Check "073" originDay
            liftIO $ length r `shouldBe` 2

            zstP <- liftIO $ getzst utcOriginP
            CheckResult r <- runParsedAction zstP (pilotUser sd) $ Check "073" originDayP
            liftIO $ length r `shouldBe` 0

            CheckResult r <- runParsedAction zstP (officerUser sd) $ Check "073" originDay
            liftIO $ length r `shouldBe` 3

            zst' <- liftIO $ getzst utcOrigin'
            CheckResult r <- runParsedAction zst' (pilotUser sd) $ Check "349" originDay
            liftIO $ length r `shouldBe` 1
        it "throws on invalid entries" $ runInDb $ \sd -> do
            zst <- liftIO $ getzst utcOrigin
            ex <- (try (runParsedAction zst (pilotUser sd) $ Check "074" originDay)) :: S.SqlM (Either FormatException ParsedActionResult)
            liftIO $ isLeft ex `shouldBe` True

            ex <- (try (runParsedAction zst (naUser sd) $ Check "073" originDay)) :: S.SqlM (Either UnauthorizedException ParsedActionResult)
            liftIO $ isLeft ex `shouldBe` True

            ex <- (try (runParsedAction zst (pilotUser sd) $ Check "073" originDayP)) :: S.SqlM (Either FormatException ParsedActionResult)
            liftIO $ isLeft ex `shouldBe` True

            ex <- (try (runParsedAction zst (pilotUser sd) $ Check "5" originDay)) :: S.SqlM (Either FormatException ParsedActionResult)
            liftIO $ isLeft ex `shouldBe` True

            ex <- (try (runParsedAction zst (pilotUser sd) $ Check "" originDay)) :: S.SqlM (Either FormatException ParsedActionResult)
            liftIO $ isLeft ex `shouldBe` True
  describe "review" $ do
        it "finds matching reservations" $ runInDb $ \sd -> do
            zst <- liftIO $ getzst utcOrigin
            ReviewResult r <- runParsedAction zst (officerUser sd) $ Review originDay
            liftIO $ length r `shouldBe` 1

            zstP <- liftIO $ getzst utcOriginP
            ReviewResult r <- runParsedAction zstP (pilotUser sd) $ Review originDayP
            liftIO $ length r `shouldBe` 0

            ReviewResult r <- runParsedAction zstP (pilotUser sd) $ Review originDay
            liftIO $ length r `shouldBe` 2

            ReviewResult r <- runParsedAction zstP (officerUser sd) $ Review originDay'
            liftIO $ length r `shouldBe` 2
        it "throws on invalid entries" $ runInDb $ \sd -> do
            zst <- liftIO $ getzst utcOrigin
            ex <- (try (runParsedAction zst (naUser sd) $ Review originDay)) :: S.SqlM (Either UnauthorizedException ParsedActionResult)
            liftIO $ isLeft ex `shouldBe` True

            ex <- (try (runParsedAction zst (pilotUser sd) $ Review originDayP)) :: S.SqlM (Either FormatException ParsedActionResult)
            liftIO $ isLeft ex `shouldBe` True
  describe "reserve" $ do
        it "reserves" $ runInDb $ \sd -> do
            zst <- liftIO $ getzst utcOriginP
            ReserveResult r <- runParsedAction zst (officerUser sd) $ Reserve "073" utcOriginF (addUTCTime 3600 utcOriginF)
            return ()

        it "throws on invalid entries" $ runInDb $ \sd -> do
            zst <- liftIO $ getzst utcOriginP
            ex <- (try (runParsedAction zst (naUser sd) $ Reserve "073" utcOrigin (addUTCTime 3600 utcOrigin))) :: S.SqlM (Either UnauthorizedException ParsedActionResult)
            liftIO $ isLeft ex `shouldBe` True

            ex <- (try (runParsedAction zst (socialUser sd) $ Reserve "073" utcOrigin (addUTCTime 3600 utcOrigin))) :: S.SqlM (Either UnauthorizedException ParsedActionResult)
            liftIO $ isLeft ex `shouldBe` True

            ex <- (try (runParsedAction zst (pilotUser sd) $ Reserve "073" utcOriginP (addUTCTime 3600 utcOrigin))) :: S.SqlM (Either ConflictException ParsedActionResult)
            liftIO $ isLeft ex `shouldBe` True

            ex <- (try (runParsedAction zst (pilotUser sd) $ Reserve "074" utcOrigin (addUTCTime 3600 utcOrigin))) :: S.SqlM (Either FormatException ParsedActionResult)
            liftIO $ isLeft ex `shouldBe` True
  describe "cancel" $ do
        it "finds matching reservations" $ runInDb $ \sd -> do
            zst <- liftIO $ getzst utcOriginF
            runParsedAction zst (officerUser sd) $ Reserve "073" utcOriginF (addUTCTime 3600 utcOriginF)

            CancelResult r True <- runParsedAction zst (officerUser sd) $ Cancel Nothing utcOriginF

            ex <- (try (runParsedAction zst (officerUser sd) $ Cancel Nothing utcOriginF)) :: S.SqlM (Either ConflictException ParsedActionResult)
            liftIO $ isLeft ex `shouldBe` True
        it "finds matching reservations various time" $ runInDb $ \sd -> do
            zst <- liftIO $ getzst utcOriginF
            runParsedAction zst (officerUser sd) $ Reserve "073" utcOriginF (addUTCTime 3600 utcOriginF)
            CancelResult r True <- runParsedAction zst (officerUser sd) $ Cancel Nothing utcOriginF

            runParsedAction zst (officerUser sd) $ Reserve "073" utcOriginF (addUTCTime 3600 utcOriginF)
            CancelResult r True <- runParsedAction zst (officerUser sd) $ Cancel Nothing (addUTCTime 3500 utcOriginF)

            runParsedAction zst (officerUser sd) $ Reserve "073" utcOriginF (addUTCTime 3600 utcOriginF)
            CancelResult r True <- runParsedAction zst (officerUser sd) $ Cancel Nothing (addUTCTime 1200 utcOriginF)

            runParsedAction zst (officerUser sd) $ Reserve "073" utcOriginF (addUTCTime 3600 utcOriginF)
            ex <- (try (runParsedAction zst (officerUser sd) $ Cancel Nothing (addUTCTime 3600 utcOriginF))) :: S.SqlM (Either ConflictException ParsedActionResult)
            liftIO $ isLeft ex `shouldBe` True

        it "finds matching reservations with explicit tail" $ runInDb $ \sd -> do
            zst <- liftIO $ getzst utcOriginF
            runParsedAction zst (officerUser sd) $ Reserve "073" utcOriginF (addUTCTime 3600 utcOriginF)

            ex <- (try (runParsedAction zst (officerUser sd) $ Cancel (Just "349") utcOriginF)) :: S.SqlM (Either ConflictException ParsedActionResult)
            liftIO $ isLeft ex `shouldBe` True

            CancelResult r True <- runParsedAction zst (officerUser sd) $ Cancel (Just "073") utcOriginF

            ex <- (try (runParsedAction zst (officerUser sd) $ Cancel (Just "073") utcOriginF)) :: S.SqlM (Either ConflictException ParsedActionResult)
            liftIO $ isLeft ex `shouldBe` True

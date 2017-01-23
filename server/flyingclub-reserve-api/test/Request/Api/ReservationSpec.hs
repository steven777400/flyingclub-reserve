{-# LANGUAGE OverloadedStrings #-}
module Request.Api.ReservationSpec where

import           Control.Exception.Conflict
import           Control.Exception.Unauthorized
import           Control.Monad.Catch
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Either
import           Data.Time.Calendar
import           Data.Time.Clock
import qualified Database.Persist.Audit.Operations  as A
import qualified Database.Persist.Schema            as S
import           Database.Persist.Sqlite
import           Database.Persist.Types.PhoneNumber
import           Database.Persist.Types.UserType
import           Database.Persist.Types.UUID
import           Request.Api.AuthorizedAction
import           System.Random

import           Request.Api.Reservation
import           Request.Api.User
import           Test.Hspec

anyUnauthorizedException :: Selector UnauthorizedException
anyUnauthorizedException = const True

anyConflictException :: Selector ConflictException
anyConflictException = const True

data SampleData = SampleData {
  officerUser :: Key S.User,
  pilotUser   :: Key S.User,
  naUser      :: Key S.User,
  n073        :: Key S.Airplane,
  n349        :: Key S.Airplane,
  n666        :: Key S.Airplane

}

runInDb :: (SampleData -> SqlPersistM a) -> IO a
runInDb sql = runSqlite ":memory:" $ do
    S.runAdjustedMigration
    i1 <- liftIO $ S.UserKey <$> (randomIO :: IO UUID)
    i2 <- liftIO $ S.UserKey <$> (randomIO :: IO UUID)
    i3 <- liftIO $ S.UserKey <$> (randomIO :: IO UUID)
    ia1 <- liftIO $ S.AirplaneKey <$> (randomIO :: IO UUID)
    ia2 <- liftIO $ S.AirplaneKey <$> (randomIO :: IO UUID)
    ia3 <- liftIO $ S.AirplaneKey <$> (randomIO :: IO UUID)
    insertKey i1 sampleOfficerUser
    insertKey i2 samplePilotUser
    insertKey i3 sampleNAUser
    insertKey ia1 a1
    insertKey ia2 a2
    insertKey ia3 a3
    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i2 ia1
      (UTCTime (fromGregorian 2027 01 20) (8*60*60))
      (UTCTime (fromGregorian 2027 01 20) (10*60*60))
      Nothing False ""

    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i1 ia1
      (UTCTime (fromGregorian 2027 01 20) (12*60*60))
      (UTCTime (fromGregorian 2027 01 20) (15*60*60))
      Nothing False ""

    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i2 ia1
      (UTCTime (fromGregorian 2027 01 20) (15*60*60))
      (UTCTime (fromGregorian 2027 01 20) (16*60*60))
      Nothing False ""

    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i1 ia1
      (UTCTime (fromGregorian 2027 01 20) (9*60*60))
      (UTCTime (fromGregorian 2027 01 20) (15*60*60))
      (Just $ UTCTime (fromGregorian 2027 01 20) (15*60*60)) False "" -- deleted

    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i1 ia2
      (UTCTime (fromGregorian 2027 01 20) (9*60*60))
      (UTCTime (fromGregorian 2027 01 20) (11*60*60))
      Nothing False ""
    sql (SampleData i1 i2 i3 ia1 ia2 ia3)

sampleOfficerUser = S.User "test1f" "test1l" "" Officer Nothing
samplePilotUser = S.User "test1f" "test1l" "" Pilot Nothing
sampleSocialUser = S.User "test1f" "test1l" "" Social Nothing
sampleNAUser = S.User "test1f" "test1l" "" NoAccess Nothing

a1 = S.Airplane "54073" "cessna 172" Nothing
a2 = S.Airplane "52349" "cessna 182" Nothing
a3 = S.Airplane "666ab" "cessna 666" (Just $ UTCTime (fromGregorian 2010 01 10) 0)

spec :: Spec
spec = do
  describe "getReservations" $ do
        it "finds in range" $ runInDb $ \sd -> do
            r <- runAuthorizedAction (pilotUser sd) (
              getReservations
                (UTCTime (fromGregorian 2027 01 10) (8*60*60))
                (UTCTime (fromGregorian 2027 01 19) (10*60*60)) )
            liftIO $ length r `shouldBe` 0

            r <- runAuthorizedAction (pilotUser sd) (
              getReservations
                (UTCTime (fromGregorian 2027 01 10) (8*60*60))
                (UTCTime (fromGregorian 2027 01 21) (10*60*60)) )
            liftIO $ length r `shouldBe` 4

            r <- runAuthorizedAction (pilotUser sd) (
              getReservations
                (UTCTime (fromGregorian 2027 01 10) (8*60*60))
                (UTCTime (fromGregorian 2027 01 20) (10*60*60)) )
            liftIO $ length r `shouldBe` 2

            -- match sure it doesn't pick up on the edge
            r <- runAuthorizedAction (pilotUser sd) (
              getReservations
                (UTCTime (fromGregorian 2027 01 20) (12*60*60))
                (UTCTime (fromGregorian 2027 01 20) (15*60*60)) )
            liftIO $ length r `shouldBe` 1

            r <- runAuthorizedAction (pilotUser sd) (
              getReservations
                (UTCTime (fromGregorian 2027 01 20) (12*60*60))
                (UTCTime (fromGregorian 2027 01 20) (16*60*60)) )
            liftIO $ length r `shouldBe` 2

            -- match sure it doesn't pick up on the edge
            r <- runAuthorizedAction (pilotUser sd) (
              getReservations
                (UTCTime (fromGregorian 2027 01 20) (15*60*60))
                (UTCTime (fromGregorian 2027 01 20) (17*60*60)) )
            liftIO $ length r `shouldBe` 1
  describe "getReservationsUser" $ do
        it "gets only nondeleted reservations after now" $ runInDb $ \sd -> do
          orig <- runAuthorizedAction (pilotUser sd) (
            getReservationsUser (pilotUser sd))
          rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
          insertKey rk $ S.Reservation
            (pilotUser sd) (n073 sd)
            (UTCTime (fromGregorian 2016 01 20) (8*60*60))
            (UTCTime (fromGregorian 2016 01 20) (10*60*60))
            Nothing False ""
          rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
          insertKey rk $ S.Reservation
            (pilotUser sd) (n073 sd)
            (UTCTime (fromGregorian 2018 01 20) (8*60*60))
            (UTCTime (fromGregorian 2018 01 20) (10*60*60))
            (Just $ UTCTime (fromGregorian 2018 01 20) 0) False ""
          r <- runAuthorizedAction (pilotUser sd) (
            getReservationsUser (pilotUser sd))
          liftIO $ length r `shouldBe` length orig

  describe "getReservationsDeleted" $ do
        it "disallows nonofficers" $ (runInDb $ \sd ->
            runAuthorizedAction (pilotUser sd) (
              getReservationsDeleted
                (UTCTime (fromGregorian 2027 01 10) (8*60*60))
                (UTCTime (fromGregorian 2027 01 19) (10*60*60)) )
            ) `shouldThrow` anyUnauthorizedException
        it "finds in range" $ runInDb $ \sd -> do
            r <- runAuthorizedAction (officerUser sd) (
              getReservationsDeleted
                (UTCTime (fromGregorian 2027 01 10) (8*60*60))
                (UTCTime (fromGregorian 2027 01 19) (10*60*60)) )
            liftIO $ length r `shouldBe` 0

            r <- runAuthorizedAction (officerUser sd) (
              getReservationsDeleted
                (UTCTime (fromGregorian 2027 01 10) (8*60*60))
                (UTCTime (fromGregorian 2027 01 21) (10*60*60)) )
            liftIO $ length r `shouldBe` 5

            r <- runAuthorizedAction (officerUser sd) (
              getReservationsDeleted
                (UTCTime (fromGregorian 2027 01 10) (8*60*60))
                (UTCTime (fromGregorian 2027 01 20) (10*60*60)) )
            liftIO $ length r `shouldBe` 3

            -- match sure it doesn't pick up on the edge
            r <- runAuthorizedAction (officerUser sd) (
              getReservationsDeleted
                (UTCTime (fromGregorian 2027 01 20) (12*60*60))
                (UTCTime (fromGregorian 2027 01 20) (15*60*60)) )
            liftIO $ length r `shouldBe` 2

            r <- runAuthorizedAction (officerUser sd) (
              getReservationsDeleted
                (UTCTime (fromGregorian 2027 01 20) (12*60*60))
                (UTCTime (fromGregorian 2027 01 20) (16*60*60)) )
            liftIO $ length r `shouldBe` 3

            -- match sure it doesn't pick up on the edge
            r <- runAuthorizedAction (officerUser sd) (
              getReservationsDeleted
                (UTCTime (fromGregorian 2027 01 20) (15*60*60))
                (UTCTime (fromGregorian 2027 01 20) (17*60*60)) )
            liftIO $ length r `shouldBe` 1
  describe "createReservation" $ do
        it "officer reserves for self and others" $ runInDb $ \sd -> do
            r <- runAuthorizedAction (pilotUser sd) (
              getReservations
                (UTCTime (fromGregorian 2027 01 21) (7*60*60))
                (UTCTime (fromGregorian 2027 01 21) (17*60*60)) )
            liftIO $ length r `shouldBe` 0

            -- do insert
            runAuthorizedAction (officerUser sd) (
              createReservation $ S.Reservation
                (pilotUser sd) (n073 sd)
                (UTCTime (fromGregorian 2027 01 21) (8*60*60))
                (UTCTime (fromGregorian 2027 01 21) (10*60*60))
                Nothing False "")

            -- confirm insert
            r <- runAuthorizedAction (pilotUser sd) (
              getReservations
                (UTCTime (fromGregorian 2027 01 21) (7*60*60))
                (UTCTime (fromGregorian 2027 01 21) (17*60*60)) )
            liftIO $ length r `shouldBe` 1

            -- do insert
            runAuthorizedAction (officerUser sd) (
              createReservation $ S.Reservation
                (officerUser sd) (n073 sd)
                (UTCTime (fromGregorian 2027 01 21) (10*60*60))
                (UTCTime (fromGregorian 2027 01 21) (12*60*60))
                Nothing False "")

            -- confirm insert
            r <- runAuthorizedAction (pilotUser sd) (
              getReservations
                (UTCTime (fromGregorian 2027 01 21) (7*60*60))
                (UTCTime (fromGregorian 2027 01 21) (17*60*60)) )
            liftIO $ length r `shouldBe` 2

            -- do insert
            runAuthorizedAction (officerUser sd) (
              createReservation $ S.Reservation
                (pilotUser sd) (n349 sd)
                (UTCTime (fromGregorian 2027 01 21) (10*60*60))
                (UTCTime (fromGregorian 2027 01 21) (12*60*60))
                Nothing False "")

            -- confirm insert
            r <- runAuthorizedAction (pilotUser sd) (
              getReservations
                (UTCTime (fromGregorian 2027 01 21) (7*60*60))
                (UTCTime (fromGregorian 2027 01 21) (17*60*60)) )
            liftIO $ length r `shouldBe` 3
        it "doesn't insert overlap reservation" $ runInDb $ \sd -> do
          -- do insert
          runAuthorizedAction (officerUser sd) (
            createReservation $ S.Reservation
              (pilotUser sd) (n073 sd)
              (UTCTime (fromGregorian 2027 01 21) (8*60*60))
              (UTCTime (fromGregorian 2027 01 21) (10*60*60))
              Nothing False "")
          -- do insert
          ex <- (try (runAuthorizedAction (officerUser sd) (
            createReservation $ S.Reservation
              (officerUser sd) (n073 sd)
              (UTCTime (fromGregorian 2027 01 21) (9*60*60))
              (UTCTime (fromGregorian 2027 01 21) (11*60*60))
              Nothing False ""))) :: S.SqlM (Either ConflictException (Key S.Reservation))
          liftIO $ isLeft ex `shouldBe` True
          -- confirm NO insert
          r <- runAuthorizedAction (pilotUser sd) (
            getReservations
              (UTCTime (fromGregorian 2027 01 21) (7*60*60))
              (UTCTime (fromGregorian 2027 01 21) (17*60*60)) )
          liftIO $ length r `shouldBe` 1
        it "throws on overlap reservation" $ (runInDb $ \sd -> do
            -- do insert
            runAuthorizedAction (officerUser sd) (
              createReservation $ S.Reservation
                (pilotUser sd) (n073 sd)
                (UTCTime (fromGregorian 2027 01 21) (8*60*60))
                (UTCTime (fromGregorian 2027 01 21) (10*60*60))
                Nothing False "")
            -- do insert
            runAuthorizedAction (officerUser sd) (
              createReservation $ S.Reservation
                (officerUser sd) (n073 sd)
                (UTCTime (fromGregorian 2027 01 21) (9*60*60))
                (UTCTime (fromGregorian 2027 01 21) (11*60*60))
                Nothing False "")
              ) `shouldThrow` anyConflictException
        it "pilot reserves for self only" $ runInDb $ \sd -> do
            r <- runAuthorizedAction (pilotUser sd) (
              getReservations
                (UTCTime (fromGregorian 2027 01 21) (7*60*60))
                (UTCTime (fromGregorian 2027 01 21) (17*60*60)) )
            liftIO $ length r `shouldBe` 0

            -- do insert
            runAuthorizedAction (pilotUser sd) (
              createReservation $ S.Reservation
                (pilotUser sd) (n073 sd)
                (UTCTime (fromGregorian 2027 01 21) (8*60*60))
                (UTCTime (fromGregorian 2027 01 21) (10*60*60))
                Nothing False "")
            -- confirm insert
            r <- runAuthorizedAction (pilotUser sd) (
              getReservations
                (UTCTime (fromGregorian 2027 01 21) (7*60*60))
                (UTCTime (fromGregorian 2027 01 21) (17*60*60)) )
            liftIO $ length r `shouldBe` 1

            -- do insert
            ex <- (try (runAuthorizedAction (pilotUser sd) (
              createReservation $ S.Reservation
                (officerUser sd) (n073 sd)
                (UTCTime (fromGregorian 2027 01 21) (14*60*60))
                (UTCTime (fromGregorian 2027 01 21) (16*60*60))
                Nothing False ""))) :: S.SqlM (Either UnauthorizedException (Key S.Reservation))
            liftIO $ isLeft ex `shouldBe` True
            -- confirm NO insert
            r <- runAuthorizedAction (pilotUser sd) (
              getReservations
                (UTCTime (fromGregorian 2027 01 21) (7*60*60))
                (UTCTime (fromGregorian 2027 01 21) (17*60*60)) )
            liftIO $ length r `shouldBe` 1

        it "throws for na users" $ (runInDb $ \sd -> do
            runAuthorizedAction (naUser sd) (createReservation $ S.Reservation
              (naUser sd) (n073 sd)
              (UTCTime (fromGregorian 2027 01 19) (8*60*60))
              (UTCTime (fromGregorian 2027 01 19) (10*60*60))
              Nothing False "")
            ) `shouldThrow` anyUnauthorizedException

        it "throws for past start" $ (runInDb $ \sd -> do
            runAuthorizedAction (officerUser sd) (createReservation $ S.Reservation
              (officerUser sd) (n073 sd)
              (UTCTime (fromGregorian 2016 01 19) (8*60*60))
              (UTCTime (fromGregorian 2016 01 19) (10*60*60))
              Nothing False "")
            ) `shouldThrow` anyConflictException
        it "throws for end before start" $ (runInDb $ \sd -> do
            runAuthorizedAction (officerUser sd) (createReservation $ S.Reservation
              (officerUser sd) (n073 sd)
              (UTCTime (fromGregorian 2027 01 19) (8*60*60))
              (UTCTime (fromGregorian 2026 01 19) (10*60*60))
              Nothing False "")
            ) `shouldThrow` anyConflictException
        it "throws for deleted airplane" $ (runInDb $ \sd -> do
            runAuthorizedAction (officerUser sd) (createReservation $ S.Reservation
              (officerUser sd) (n666 sd)
              (UTCTime (fromGregorian 2027 01 19) (8*60*60))
              (UTCTime (fromGregorian 2027 01 19) (10*60*60))
              Nothing False "")
            ) `shouldThrow` anyException
        it "disallows pilot to make maint res" $ (runInDb $ \sd -> do
            runAuthorizedAction (pilotUser sd) (createReservation $ S.Reservation
              (pilotUser sd) (n073 sd)
              (UTCTime (fromGregorian 2027 01 19) (8*60*60))
              (UTCTime (fromGregorian 2027 01 19) (10*60*60))
              Nothing True "")
            ) `shouldThrow` anyConflictException
        it "allows officer to make maint res" $ runInDb $ \sd -> do
            runAuthorizedAction (officerUser sd) (createReservation $ S.Reservation
              (officerUser sd) (n073 sd)
              (UTCTime (fromGregorian 2027 01 19) (8*60*60))
              (UTCTime (fromGregorian 2027 01 19) (10*60*60))
              Nothing True "")
            r <- runAuthorizedAction (pilotUser sd) (
              getReservations
                (UTCTime (fromGregorian 2027 01 19) (7*60*60))
                (UTCTime (fromGregorian 2027 01 19) (17*60*60)) )
            liftIO $ length r `shouldBe` 1

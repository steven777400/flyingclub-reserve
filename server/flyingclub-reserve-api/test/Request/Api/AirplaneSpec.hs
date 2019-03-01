{-# LANGUAGE OverloadedStrings #-}
module Request.Api.AirplaneSpec where

import           Control.Exception.Conflict
import           Control.Exception.Unauthorized
import           Control.Monad.Catch
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Either
import           Data.Time.Calendar
import           Data.Time.Clock
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

import           Request.Api.Airplane
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
    let rkr = S.Reservation i2 ia1 (UTCTime (fromGregorian 2027 01 20) (8*60*60)) (UTCTime (fromGregorian 2027 01 20) (10*60*60)) False "" Nothing 
    insertKey rk rkr
    let rk1 = rk

    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i1 ia1
      (UTCTime (fromGregorian 2027 01 20) (12*60*60))
      (UTCTime (fromGregorian 2027 01 20) (15*60*60))
      False "" Nothing 
    let ork = rk

    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i2 ia1
      (UTCTime (fromGregorian 2027 01 20) (15*60*60))
      (UTCTime (fromGregorian 2027 01 20) (16*60*60))
      False ""
      Nothing 

    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i1 ia1
      (UTCTime (fromGregorian 2027 01 20) (9*60*60))
      (UTCTime (fromGregorian 2027 01 20) (15*60*60))
      False ""
      (Just $ UTCTime (fromGregorian 2027 01 20) (15*60*60))  -- deleted

    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i1 ia2
      (UTCTime (fromGregorian 2027 01 20) (9*60*60))
      (UTCTime (fromGregorian 2027 01 20) (11*60*60))
      False "" Nothing
    sql (SampleData i1 i2 i3 ia1 ia2 ia3 rk1 ork)

sampleOfficerUser = S.User "test1f" "test1l" Officer (fromGregorian 1990 1 1) Nothing
samplePilotUser = S.User "test1f" "test1l" Pilot (fromGregorian 1990 1 1) Nothing
sampleSocialUser = S.User "test1f" "test1l" Social (fromGregorian 1990 1 1) Nothing
sampleNAUser = S.User "test1f" "test1l" NoAccess (fromGregorian 1990 1 1) Nothing

a1 = S.Airplane "540B3" "cessna 172" Nothing
a2 = S.Airplane "523B3" "cessna 182" Nothing
a3 = S.Airplane "66673" "cessna 666" (Just $ UTCTime (fromGregorian 2010 01 10) 0)

spec :: Spec
spec = do
  describe "getAirplanes" $ do
        it "finds all" $ runInDb $ \sd -> do
            r <- runAuthorizedAction (pilotUser sd)
              getAirplanes
            liftIO $ length r `shouldBe` 2 -- one is deleted!
        it "disallows na users" $ (runInDb $ \sd ->
            runAuthorizedAction (naUser sd)
              getAirplanes
            ) `shouldThrow` anyUnauthorizedException
  describe "getAirplanesDeleted" $ do
        it "finds all" $ runInDb $ \sd -> do
            r <- runAuthorizedAction (officerUser sd)
              getAirplanesDeleted
            liftIO $ length r `shouldBe` 3
        it "disallows pilot users" $ (runInDb $ \sd ->
            runAuthorizedAction (pilotUser sd)
              getAirplanesDeleted
            ) `shouldThrow` anyUnauthorizedException
  describe "findAirplanes" $ do
        it "finds multiple matching" $ runInDb $ \sd -> do
            r <- runAuthorizedAction (pilotUser sd) $
              findAirplanes "B3"
            liftIO $ length r `shouldBe` 2 -- one is deleted
        it "finds individual matching" $ runInDb $ \sd -> do
            r <- runAuthorizedAction (pilotUser sd) $
              findAirplanes "0B3"
            liftIO $ length r `shouldBe` 1
        it "finds no matching" $ runInDb $ \sd -> do
            r <- runAuthorizedAction (pilotUser sd) $
              findAirplanes "n0B3"
            liftIO $ length r `shouldBe` 0
        it "finds individual matching case insensitive" $ runInDb $ \sd -> do
            r <- runAuthorizedAction (pilotUser sd) $
              findAirplanes "0b3"
            liftIO $ length r `shouldBe` 1
        it "disallows na users" $ (runInDb $ \sd ->
            runAuthorizedAction (naUser sd) $
              findAirplanes "73"
            ) `shouldThrow` anyUnauthorizedException

{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.AuditSpec where

import           Control.Monad.Trans
import           Data.Aeson
import           Data.Time.Calendar
import           Data.Time.Clock
import           Database.Persist.Environment.Sqlite (runInMemory)
import           Database.Persist.Schema
import           Database.Persist.Sqlite
import           Database.Persist.Types.UserType
import           Database.Persist.Types.UUID
import           System.Random

import qualified Database.Persist.Audit.Operations   as A
import           Test.Hspec

runInDb :: SqlM a -> IO a
runInDb sql = runInMemory $ do
    runAdjustedMigration
    sql

sampleUser = User "test1f" "test1l" Officer (fromGregorian 1990 1 1) Nothing

spec :: Spec
spec = do
    describe "basic audit" $ do
        it "tracks inserts" $ runInDb $ do
            i1 <- liftIO $ UserKey <$> (randomIO :: IO UUID)
            insertKey i1 sampleUser
            i2 <- liftIO $ UserKey <$> (randomIO :: IO UUID)
            insertKey i2 sampleUser
            users <- selectList [] [Asc UserId]
            liftIO $ length users `shouldBe` 2
            ak <- A.insert i1 $ Airplane "N54073" "Goat!" Nothing
            airplanes <- selectList [] [Asc AirplaneId]
            liftIO $ length airplanes `shouldBe` 1
            t <- liftIO getCurrentTime
            A.insert i1 $ Reservation i1 ak t t False "test" Nothing
            let rv2data = Reservation i2 ak t t False "test" Nothing
            rv <- A.insert i2 $ rv2data
            audit <- selectList [] [Asc AuditId]
            liftIO $ length audit `shouldBe` 3
            auditi1 <- selectList [AuditUserId ==. i1] [Asc AuditId]
            liftIO $ length auditi1 `shouldBe` 2
            auditi2 <- selectList [AuditUserId ==. i2] [Asc AuditId]
            liftIO $ length auditi2 `shouldBe` 1
            let (Entity audite auditi2') = head auditi2
            liftIO $ auditObjectId auditi2' `shouldBe` unReservationKey rv
            liftIO $ auditOriginal auditi2' `shouldBe` Nothing
            liftIO $ auditNew auditi2' `shouldBe` (Just $ toJSON rv2data)

        it "tracks deletes" $ runInDb $ do
            i1 <- liftIO $ UserKey <$> (randomIO :: IO UUID)
            insertKey i1 sampleUser
            i2 <- liftIO $ UserKey <$> (randomIO :: IO UUID)
            insertKey i2 sampleUser
            i3 <- liftIO $ UserKey <$> (randomIO :: IO UUID)
            insertKey i3 sampleUser
            ak <- A.insert i1 $ Airplane "N54073" "Goat!" Nothing
            t <- liftIO getCurrentTime
            r1 <- A.insert i1 $ Reservation i1 ak t t False "" Nothing
            r2 <- A.insert i2 $ Reservation i2 ak t t False "" Nothing
            let r3data = Reservation i2 ak t t False "" Nothing
            r3 <- A.insert i2 $ r3data
            ndres1 <- selectList [ReservationDeleted ==. Nothing] []
            liftIO $ length ndres1 `shouldBe` 3
            A.delete i3 r2
            ndres2 <- selectList [ReservationDeleted ==. Nothing] []
            liftIO $ length ndres2 `shouldBe` 2
            dres2 <- selectList [ReservationDeleted !=. Nothing] []
            liftIO $ length dres2 `shouldBe` 1
            let (Entity kres2 _) = head dres2
            liftIO $ unReservationKey kres2 `shouldBe` unReservationKey r2
            auditi3 <- selectList [AuditUserId ==. i3] [Asc AuditId]
            liftIO $ length auditi3 `shouldBe` 1
            let (Entity _ auditi3') = head auditi3
            liftIO $ auditObjectId auditi3' `shouldBe` unReservationKey r2
            liftIO $ auditOriginal auditi3' `shouldBe` (Just $ toJSON r3data)
            liftIO $ auditNew auditi3' `shouldBe` Nothing

        it "throws double deletes" $ (runInDb $ do
            i1 <- liftIO $ UserKey <$> (randomIO :: IO UUID)
            insertKey i1 sampleUser
            ak <- A.insert i1 $ Airplane "N54073" "Goat!" Nothing
            t <- liftIO getCurrentTime
            r1 <- A.insert i1 $ Reservation i1 ak t t False "" Nothing
            A.delete i1 r1
            A.delete i1 r1
            ) `shouldThrow` anyException -- can't delete again

        it "throws non-existent delete" $ (runInDb $ do
            i1 <- liftIO $ UserKey <$> (randomIO :: IO UUID)
            insertKey i1 sampleUser
            r1 <- liftIO $ ReservationKey <$> (randomIO :: IO UUID)
            A.delete i1 r1
            ) `shouldThrow` anyException

        it "tracks updates" $ runInDb $ do
            i1 <- liftIO $ UserKey <$> (randomIO :: IO UUID)
            insertKey i1 sampleUser
            i2 <- liftIO $ UserKey <$> (randomIO :: IO UUID)
            insertKey i2 sampleUser
            ak1 <- A.insert i1 $ Airplane "N54073" "Goat!" Nothing
            ak2 <- A.insert i1 $ Airplane "N75898" "Goat!" Nothing
            t <- liftIO getCurrentTime
            let t' = addUTCTime (10000) t
            let r1data = Reservation i1 ak2 t t False "" Nothing
            r1 <- A.insert i1 $ r1data
            let r2data = Reservation i2 ak2 t t' False "" Nothing
            r2 <- A.update i2 r1 [ReservationUserId =. i2, ReservationAirplaneId =. ak2, ReservationEnd =. t']
            liftIO $ (toJSON r2data) `shouldBe` (toJSON r2)
            auditi2 <- selectList [AuditUserId ==. i2] [Asc AuditId]
            liftIO $ length auditi2 `shouldBe` 1
            let (Entity _ auditi2') = head auditi2
            liftIO $ auditObjectId auditi2' `shouldBe` unReservationKey r1
            liftIO $ auditOriginal auditi2' `shouldBe` (Just $ toJSON r1data)
            liftIO $ auditNew auditi2' `shouldBe` (Just $ toJSON r2data)

        it "throws non-existent update" $ (runInDb $ do
            i1 <- liftIO $ UserKey <$> (randomIO :: IO UUID)
            insertKey i1 sampleUser
            r1 <- liftIO $ ReservationKey <$> (randomIO :: IO UUID)
            A.update i1 r1 [ReservationUserId =. i1]
            ) `shouldThrow` anyException

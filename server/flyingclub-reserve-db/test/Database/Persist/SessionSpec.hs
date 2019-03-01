{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.SessionSpec where

import           Control.Monad.Trans
import           Data.Aeson
import           Data.Maybe
import           Data.Time.Calendar
import           Data.Time.Clock
import           Database.Persist.Environment.Sqlite (runInMemory)
import           Database.Persist.Schema
import           Database.Persist.Sqlite
import           Database.Persist.Types.PhoneNumber
import           Database.Persist.Types.PIN
import           Database.Persist.Types.UserType
import           Database.Persist.Types.UUID
import           System.Random

import qualified Database.Persist.Session            as S
import           Test.Hspec

runInDb :: SqlM a -> IO a
runInDb sql = runInMemory $ do
    runAdjustedMigration
    sql

sampleUser1 = User "test1f" "test1l" Officer (fromGregorian 1990 1 1) Nothing
sampleUser2 = User "test2f" "test2l" Social (fromGregorian 1990 1 1) Nothing
sampleUser3 = User "test3f" "test3l" NoAccess (fromGregorian 1990 1 1) Nothing

prepDb = do
    i1 <- liftIO $ UserKey <$> (randomIO :: IO UUID)
    insertKey i1 sampleUser1
    e1 <- liftIO $ EmailKey <$> (randomIO :: IO UUID)
    insertKey e1 $ Email i1 "" "steve@kolls.net" True True Nothing
    p1 <- liftIO $ PhoneKey <$> (randomIO :: IO UUID)
    insertKey p1 $ Phone i1 "" (toPhoneNumber "(360) 555-1234") True True Nothing
    a1 <- liftIO $ AuthenticationKey <$> (randomIO :: IO UUID)
    insertKey a1 $ Authentication i1 0 (toPIN "1234")
-- pin 1234

    i2 <- liftIO $ UserKey <$> (randomIO :: IO UUID)
    insertKey i2 sampleUser2
    e2 <- liftIO $ EmailKey <$> (randomIO :: IO UUID)
    insertKey e2 $ Email i2 "" "user@example.com" True True Nothing
    p2 <- liftIO $ PhoneKey <$> (randomIO :: IO UUID)
    insertKey p2 $ Phone i2 "" (toPhoneNumber "1234567890") True True Nothing
    a2 <- liftIO $ AuthenticationKey <$> (randomIO :: IO UUID)
    insertKey a2 $ Authentication i2 0 (toPIN "9876")
-- pin 9876

    i3 <- liftIO $ UserKey <$> (randomIO :: IO UUID)
    insertKey i3 sampleUser3
    e3 <- liftIO $ EmailKey <$> (randomIO :: IO UUID)
    insertKey e3 $ Email i3 "" "na@example.com" True True Nothing
    a3 <- liftIO $ AuthenticationKey <$> (randomIO :: IO UUID)
    insertKey a3 $ Authentication i3 0 (toPIN "9876")
    -- pin 9876
    return (i1, i2, i3)

spec :: Spec
spec = do
    describe "login" $ do
        it "rejects no matching username" $ runInDb $ do
            prepDb
            session1 <- S.login "x@y.net" ""
            liftIO $ isNothing session1 `shouldBe` True
            session2 <- S.login "9876543210" ""
            liftIO $ isNothing session2 `shouldBe` True
        it "rejects no username/pw" $ runInDb $ do
            prepDb
            session <- S.login "" ""
            liftIO $ isNothing session `shouldBe` True
        it "rejects wrong pw" $ runInDb $ do
            (i1, i2, _) <- prepDb
            auth <- entityVal <$> fromJust <$> getBy (AuthUserId i1)
            liftIO $ authenticationFailedLoginCount auth `shouldBe` 0
            session <- S.login "steve@kolls.net" ""
            liftIO $ isNothing session `shouldBe` True
            auth <- entityVal <$> fromJust <$> getBy (AuthUserId i1)
            liftIO $ authenticationFailedLoginCount auth `shouldBe` 1
            session <- S.login "steve@kolls.net" "12345"
            liftIO $ isNothing session `shouldBe` True
            auth <- entityVal <$> fromJust <$> getBy (AuthUserId i1)
            liftIO $ authenticationFailedLoginCount auth `shouldBe` 2
        it "accepts right pw" $ runInDb $ do
            (i1, i2, _) <- prepDb
            auth <- entityVal <$> fromJust <$> getBy (AuthUserId i1)
            liftIO $ authenticationFailedLoginCount auth `shouldBe` 0
            session <- S.login "steve@kolls.net" "1234"
            liftIO $ isNothing session `shouldBe` False
            auth <- entityVal <$> fromJust <$> getBy (AuthUserId i1)
            liftIO $ authenticationFailedLoginCount auth `shouldBe` 0
            session <- S.login "steve@kolls.net" "12345"
            liftIO $ isNothing session `shouldBe` True
            auth <- entityVal <$> fromJust <$> getBy (AuthUserId i1)
            liftIO $ authenticationFailedLoginCount auth `shouldBe` 1
            -- a correct login should reset failed count if not locked
            session <- S.login "steve@kolls.net" "1234"
            liftIO $ isNothing session `shouldBe` False
            auth <- entityVal <$> fromJust <$> getBy (AuthUserId i1)
            liftIO $ authenticationFailedLoginCount auth `shouldBe` 0
        it "locks out accounts" $ runInDb $ do
            (i1, i2, _) <- prepDb
            auth <- entityVal <$> fromJust <$> getBy (AuthUserId i1)
            liftIO $ authenticationFailedLoginCount auth `shouldBe` 0
            session <- S.login "steve@kolls.net" "12345"
            session <- S.login "steve@kolls.net" "12345"
            session <- S.login "steve@kolls.net" "12345"
            session <- S.login "steve@kolls.net" "12345"
            session <- S.login "steve@kolls.net" "12345"
            session <- S.login "steve@kolls.net" "12345"
            session <- S.login "steve@kolls.net" "12345"
            session <- S.login "steve@kolls.net" "12345"
            session <- S.login "steve@kolls.net" "12345"
            session <- S.login "steve@kolls.net" "12345"
            auth <- entityVal <$> fromJust <$> getBy (AuthUserId i1)
            liftIO $ authenticationFailedLoginCount auth `shouldBe` 10
            session <- S.login "steve@kolls.net" "1234"
            liftIO $ isNothing session `shouldBe` True
            auth <- entityVal <$> fromJust <$> getBy (AuthUserId i1)
            liftIO $ authenticationFailedLoginCount auth `shouldBe` 10
            session <- S.login "steve@kolls.net" "1234"
            liftIO $ isNothing session `shouldBe` True
            auth <- entityVal <$> fromJust <$> getBy (AuthUserId i1)
            liftIO $ authenticationFailedLoginCount auth `shouldBe` 10
        it "gets valid sessions" $ runInDb $ do
            (i1, i2, _) <- prepDb
            (Just s1) <- S.login "steve@kolls.net" "1234"
            (Just s2) <- S.login "steve@kolls.net" "1234"
            s1' <- S.getValidSession (entityKey s1)
            s2' <- S.getValidSession (entityKey s2)
            liftIO $ isNothing s1' `shouldBe` False
            liftIO $ isNothing s2' `shouldBe` False
            liftIO $ toJSON s1 `shouldBe` toJSON s1'
            liftIO $ toJSON s2 `shouldBe` toJSON s2'
            liftIO $ toJSON s1 `shouldNotBe` toJSON s2
        it "handles logout" $ runInDb $ do
            (i1, i2, _) <- prepDb
            (Just s1a) <- S.login "steve@kolls.net" "1234"
            (Just s1b) <- S.login "steve@kolls.net" "1234"
            (Just s2) <- S.login "user@example.com" "9876"
            s1a' <- S.getValidSession (entityKey s1a)
            s1b' <- S.getValidSession (entityKey s1b)
            s2' <- S.getValidSession (entityKey s2)
            liftIO $ isNothing s1a' `shouldBe` False
            liftIO $ isNothing s1b' `shouldBe` False
            liftIO $ isNothing s2' `shouldBe` False
            S.logout (entityKey s1b)
            s1a' <- S.getValidSession (entityKey s1a)
            s1b' <- S.getValidSession (entityKey s1b)
            s2' <- S.getValidSession (entityKey s2)
            liftIO $ isNothing s1a' `shouldBe` False
            liftIO $ isNothing s1b' `shouldBe` True
            liftIO $ isNothing s2' `shouldBe` False
        it "handles logoutAll" $ runInDb $ do
            (i1, i2, _) <- prepDb
            (Just s1a) <- S.login "steve@kolls.net" "1234"
            (Just s1b) <- S.login "steve@kolls.net" "1234"
            (Just s2) <- S.login "user@example.com" "9876"
            s1a' <- S.getValidSession (entityKey s1a)
            s1b' <- S.getValidSession (entityKey s1b)
            s2' <- S.getValidSession (entityKey s2)
            liftIO $ isNothing s1a' `shouldBe` False
            liftIO $ isNothing s1b' `shouldBe` False
            liftIO $ isNothing s2' `shouldBe` False
            S.logoutAll ((sessionUserId.entityVal) s1b)
            s1a' <- S.getValidSession (entityKey s1a)
            s1b' <- S.getValidSession (entityKey s1b)
            s2' <- S.getValidSession (entityKey s2)
            liftIO $ isNothing s1a' `shouldBe` True
            liftIO $ isNothing s1b' `shouldBe` True
            liftIO $ isNothing s2' `shouldBe` False
        it "refuses na users" $ runInDb $ do
            (i1, i2, i3) <- prepDb
            s3a <- S.login "na@example.com" "9876"
            liftIO $ isNothing s3a `shouldBe` True
        it "refuses deleted users" $ runInDb $ do
            (i1, i2, i3) <- prepDb
            -- delete i1, we don't delete, we mark deleted
            now <- liftIO getCurrentTime
            update i1 [UserDeleted =. Just now]
            s1a <- S.login "steve@kolls.net" "1234"
            liftIO $ isNothing s1a `shouldBe` True

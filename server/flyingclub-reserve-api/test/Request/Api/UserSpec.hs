{-# LANGUAGE OverloadedStrings #-}
module Request.Api.UserSpec where

import           Control.Exception.Unauthorized
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Time.Clock
import qualified Database.Persist.Audit.Operations  as A
import qualified Database.Persist.Schema            as S
import           Database.Persist.Sqlite
import           Database.Persist.Types.PhoneNumber
import           Database.Persist.Types.UserType
import           Database.Persist.Types.UUID
import           Request.Api.AuthorizedAction
import           System.Random

import           Request.Api.User
import           Test.Hspec

anyUnauthorizedException :: Selector UnauthorizedException
anyUnauthorizedException = const True

runInDb :: SqlPersistM a -> IO a
runInDb sql = runSqlite ":memory:" $ do
    S.runAdjustedMigration
    sql

sampleOfficerUser = S.User "test1f" "test1l" Officer Nothing
samplePilotUser = S.User "test1f" "test1l" Pilot Nothing
sampleSocialUser = S.User "test1f" "test1l" Social Nothing
sampleNAUser = S.User "test1f" "test1l" NoAccess Nothing

sampleAllowAddr userId = S.Address userId "allow test" "" "" "" True Nothing
sampleDisallowAddr userId = S.Address userId "disallow test" "" "" "" False Nothing

sampleAllowEmail userId = S.Email userId "allow email" "test@test.com" True True Nothing
sampleDisallowEmail userId = S.Email userId "disallow email" "a@b.com" True False Nothing

sampleAllowPhone userId = S.Phone userId "allow ph" (toPhoneNumber "3605551212") True True Nothing
sampleDisallowPhone userId = S.Phone userId "disallow ph" (toPhoneNumber "3605551212") True False Nothing


spec :: Spec
spec = do
    describe "getUsers" $ do
        it "returns users" $ runInDb $ do
            i1 <- liftIO $ S.UserKey <$> (randomIO :: IO UUID)
            insertKey i1 sampleOfficerUser
            users <- runAuthorizedAction i1 getUsers
            liftIO $ length users `shouldBe` 1

            i2 <- liftIO $ S.UserKey <$> (randomIO :: IO UUID)
            insertKey i2 samplePilotUser
            users2 <- runAuthorizedAction i1 getUsers
            liftIO $ length users2 `shouldBe` 2

            A.delete i1 i2

            users3 <- runAuthorizedAction i2 getUsers -- with pilot
            liftIO $ length users3 `shouldBe` 1

            users3' <- runAuthorizedAction i1 getUsers -- with officer
            liftIO $ length users3' `shouldBe` 1

        it "throws for na users" $ (runInDb $ do
            i3 <- liftIO $ S.UserKey <$> (randomIO :: IO UUID)
            insertKey i3 sampleNAUser
            runAuthorizedAction i3 getUsers
            ) `shouldThrow` anyUnauthorizedException
    describe "getUserDetails" $ do
        it "returns full details for officer" $ runInDb $ do
            i0 <- liftIO $ S.UserKey <$> (randomIO :: IO UUID)
            insertKey i0 sampleOfficerUser

            i1 <- liftIO $ S.UserKey <$> (randomIO :: IO UUID)
            insertKey i1 samplePilotUser
            A.insert i1 (sampleAllowAddr i1)
            A.insert i1 (sampleDisallowAddr i1)

            A.insert i1 (sampleAllowEmail i1)
            A.insert i1 (sampleDisallowEmail i1)

            A.insert i1 (sampleAllowPhone i1)
            A.insert i1 (sampleDisallowPhone i1)

            det <- runAuthorizedAction i0 $ getUserDetails i1
            liftIO $ length (addressess det) `shouldBe` 2
            liftIO $ length (emails det) `shouldBe` 2
            liftIO $ length (phones det) `shouldBe` 2

        it "returns full details for owner" $ runInDb $ do
            i1 <- liftIO $ S.UserKey <$> (randomIO :: IO UUID)
            insertKey i1 samplePilotUser
            A.insert i1 (sampleDisallowAddr i1)
            A.insert i1 (sampleAllowAddr i1)
            A.insert i1 (sampleDisallowAddr i1)
            A.insert i1 (sampleDisallowAddr i1)
            A.insert i1 (sampleAllowAddr i1)
            A.insert i1 (sampleDisallowAddr i1)
            A.insert i1 (sampleDisallowAddr i1)
            A.insert i1 (sampleAllowAddr i1)

            A.insert i1 (sampleAllowEmail i1)
            A.insert i1 (sampleAllowEmail i1)
            A.insert i1 (sampleDisallowEmail i1)

            A.insert i1 (sampleAllowPhone i1)
            A.insert i1 (sampleDisallowPhone i1)
            A.insert i1 (sampleDisallowPhone i1)

            det <- runAuthorizedAction i1 $ getUserDetails i1
            liftIO $ length (addressess det) `shouldBe` 8
            liftIO $ length (emails det) `shouldBe` 3
            liftIO $ length (phones det) `shouldBe` 3

        it "returns partial details for others" $ runInDb $ do
            i0 <- liftIO $ S.UserKey <$> (randomIO :: IO UUID)
            insertKey i0 sampleSocialUser

            i1 <- liftIO $ S.UserKey <$> (randomIO :: IO UUID)
            insertKey i1 samplePilotUser
            A.insert i1 (sampleDisallowAddr i1)
            A.insert i1 (sampleAllowAddr i1)
            A.insert i1 (sampleDisallowAddr i1)
            A.insert i1 (sampleDisallowAddr i1)
            A.insert i1 (sampleAllowAddr i1)
            A.insert i1 (sampleDisallowAddr i1)
            A.insert i1 (sampleDisallowAddr i1)
            A.insert i1 (sampleAllowAddr i1)

            A.insert i1 (sampleAllowEmail i1)
            A.insert i1 (sampleAllowEmail i1)
            A.insert i1 (sampleDisallowEmail i1)

            A.insert i1 (sampleAllowPhone i1)
            A.insert i1 (sampleDisallowPhone i1)
            A.insert i1 (sampleDisallowPhone i1)

            det <- runAuthorizedAction i0 $ getUserDetails i1
            liftIO $ length (addressess det) `shouldBe` 3
            liftIO $ length (emails det) `shouldBe` 2
            liftIO $ length (phones det) `shouldBe` 1

        it "throws for na users" $ (runInDb $ do
            i0 <- liftIO $ S.UserKey <$> (randomIO :: IO UUID)
            insertKey i0 sampleNAUser

            runAuthorizedAction i0 $ getUserDetails i0
            ) `shouldThrow` anyUnauthorizedException

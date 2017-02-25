{-# LANGUAGE OverloadedStrings #-}
module Web.Api.SessionSpec where

import           Control.Monad                            (when)
import           Control.Monad.Logger
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString                          (append)
import           Data.ByteString.Char8                    (pack)
import           Data.Maybe
import           Database.Persist.Schema
import           Database.Persist.Sqlite
import           Database.Persist.Types.PhoneNumber
import           Database.Persist.Types.PIN
import           Database.Persist.Types.UserType
import           Database.Persist.Types.UUID
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Method
import           Network.Wai
import           Network.Wai.Test                         (simpleBody)
import           System.Directory
import           System.Random
import           Test.Hspec
import           Test.Hspec.Wai

import           Data.ReserveRoute
import qualified Database.Persist.Environment.Environment as DBE
import           Database.Persist.Environment.Sqlite
import           Web.Application
import qualified Web.Api.Session                              as S


sampleUser1 = User "test1f" "test1l" "" Officer Nothing
sampleUser2 = User "test2f" "test2l" "" Social Nothing

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
    return (i1, i2)

-- see https://github.com/hspec/hspec-wai#readme




app :: IO Application
app = do
  db <- runInDb
  (DBE.sql db) prepDb
  return $ application $ ReserveRoute (DBE.sql db) id

spec :: Spec
spec = with app $ do
    describe "POST /login" $ do
        it "rejects no credentials" $
            post "/login" "" `shouldRespondWith` 401
        it "rejects bad username" $
            -- steve@koll.net 1234
            request methodPost  "/login" [(hAuthorization, "Basic c3RldmVAa29sbC5uZXQ6MTIzNA==")] "" `shouldRespondWith` 401
        it "rejects bad password" $
            -- steve@kolls.net 123
            request methodPost  "/login" [(hAuthorization, "Basic c3RldmVAa29sbHMubmV0OjEyMw==")] "" `shouldRespondWith` 401
        it "accepts correct username and password" $ do
            -- steve@kolls.net 1234
            request methodPost  "/login" [(hAuthorization, "Basic c3RldmVAa29sbHMubmV0OjEyMzQ=")] "" `shouldRespondWith` 200
            -- user@example.com 9876
            request methodPost  "/login" [(hAuthorization, "Basic dXNlckBleGFtcGxlLmNvbTo5ODc2")] "" `shouldRespondWith` 200
        it "allows session resumption" $ do
          -- steve@kolls.net 1234
          r1 <- request methodPost  "/login" [(hAuthorization, "Basic c3RldmVAa29sbHMubmV0OjEyMzQ=")] ""
          let (Just json1) = decode (simpleBody r1) :: Maybe Object
          let (Just sid) = parseMaybe (\o -> o .: "id") json1 :: Maybe String
          let (Just userid1) = parseMaybe (\o -> o .: "sessionUserId") json1 :: Maybe String
          srbs <- liftIO $ (randomIO :: IO UUID)
          request methodPost  "/login" [(hAuthorization, append "Bearer " (pack $ show srbs))] "" `shouldRespondWith` 401

          request methodPost  "/login" [(hAuthorization, append "Bearer " (pack sid))] "" `shouldRespondWith` 200

          r2 <- request methodPost  "/login" [(hAuthorization, append "Bearer " (pack sid))] ""
          let (Just json2) = decode (simpleBody r2) :: Maybe Object
          let (Just sid2) = parseMaybe (\o -> o .: "id") json2 :: Maybe String
          let (Just userid2) = parseMaybe (\o -> o .: "sessionUserId") json2 :: Maybe String
          liftIO $ sid2 `shouldBe` sid
          liftIO $ userid2 `shouldBe` userid1

        it "distinguishes login" $ do
          -- steve@kolls.net 1234
          r1 <- request methodPost  "/login" [(hAuthorization, "Basic c3RldmVAa29sbHMubmV0OjEyMzQ=")] ""
          let (Just json1) = decode (simpleBody r1) :: Maybe Object
          let (Just userid1) = parseMaybe (\o -> o .: "sessionUserId") json1 :: Maybe String
          -- user@example.com 9876
          r2 <- request methodPost  "/login" [(hAuthorization, "Basic dXNlckBleGFtcGxlLmNvbTo5ODc2")] ""
          let (Just json2) = decode (simpleBody r2) :: Maybe Object
          let (Just userid2) = parseMaybe (\o -> o .: "sessionUserId") json2 :: Maybe String
          liftIO $ userid1 `shouldNotBe` userid2

          -- steve@kolls.net 1234
          r3 <- request methodPost  "/login" [(hAuthorization, "Basic c3RldmVAa29sbHMubmV0OjEyMzQ=")] ""
          let (Just json3) = decode (simpleBody r3) :: Maybe Object
          let (Just userid3) = parseMaybe (\o -> o .: "sessionUserId") json3 :: Maybe String
          liftIO $ userid1 `shouldBe` userid3

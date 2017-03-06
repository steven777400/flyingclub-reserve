{-# LANGUAGE OverloadedStrings #-}
module Web.AuthenticationSpec where

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


sampleUser1 = User "test1f" "test1l" Officer Nothing
sampleUser2 = User "test2f" "test2l" Social Nothing

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
    describe "authorizedRoute" $ do
        it "runs action if session id and key present" $ do
            -- steve@kolls.net 1234
            r1 <- request methodPost  "/login" [(hAuthorization, "Basic c3RldmVAa29sbHMubmV0OjEyMzQ=")] ""
            let (Just json1) = decode (simpleBody r1) :: Maybe Object
            let (Just sid) = parseMaybe (\o -> o .: "id") json1 :: Maybe String
            let (Just sauth) = parseMaybe (\o -> o .: "sessionAuthKey") json1 :: Maybe String
            request methodGet "/verifyAuth" [(hAuthorization, append "Bearer " (pack sid)), ("auth-key", pack sauth)] ""
              `shouldRespondWith` "\"OK\"" -- It's f--ing JSON so there are extra quotes.  Of course.
        it "403s action if session id and key present for wrong person" $ do
            -- user@example.com 9876
            r1 <- request methodPost  "/login" [(hAuthorization, "Basic dXNlckBleGFtcGxlLmNvbTo5ODc2")] ""
            let (Just json1) = decode (simpleBody r1) :: Maybe Object
            let (Just sid) = parseMaybe (\o -> o .: "id") json1 :: Maybe String
            let (Just sauth) = parseMaybe (\o -> o .: "sessionAuthKey") json1 :: Maybe String
            request methodGet "/verifyAuth" [(hAuthorization, append "Bearer " (pack sid)), ("auth-key", pack sauth)] "" `shouldRespondWith` 403
        it "401s action if session id present but key wrong" $ do
            -- steve@kolls.net 1234
            r1 <- request methodPost  "/login" [(hAuthorization, "Basic c3RldmVAa29sbHMubmV0OjEyMzQ=")] ""
            let (Just json1) = decode (simpleBody r1) :: Maybe Object
            let (Just sid) = parseMaybe (\o -> o .: "id") json1 :: Maybe String
            sauth <- liftIO $ (randomIO :: IO UUID)
            request methodGet "/verifyAuth" [(hAuthorization, append "Bearer " (pack sid)), ("auth-key", pack $ show sauth)] "" `shouldRespondWith` 401
        it "401s action if session id present but key missing" $ do
            -- steve@kolls.net 1234
            r1 <- request methodPost  "/login" [(hAuthorization, "Basic c3RldmVAa29sbHMubmV0OjEyMzQ=")] ""
            let (Just json1) = decode (simpleBody r1) :: Maybe Object
            let (Just sid) = parseMaybe (\o -> o .: "id") json1 :: Maybe String
            request methodGet "/verifyAuth" [(hAuthorization, append "Bearer " (pack sid))] "" `shouldRespondWith` 401
        it "401s action if session id wrong" $ do
            -- steve@kolls.net 1234
            r1 <- request methodPost  "/login" [(hAuthorization, "Basic c3RldmVAa29sbHMubmV0OjEyMzQ=")] ""
            let (Just json1) = decode (simpleBody r1) :: Maybe Object
            sid <- liftIO $ (randomIO :: IO UUID)
            let (Just sauth) = parseMaybe (\o -> o .: "sessionAuthKey") json1 :: Maybe String
            request methodGet "/verifyAuth" [(hAuthorization, append "Bearer " (pack $ show sid)), ("auth-key", pack sauth)] "" `shouldRespondWith` 401
        it "401s action if session id missing" $ do
            -- steve@kolls.net 1234
            r1 <- request methodPost  "/login" [(hAuthorization, "Basic c3RldmVAa29sbHMubmV0OjEyMzQ=")] ""
            let (Just json1) = decode (simpleBody r1) :: Maybe Object
            let (Just sauth) = parseMaybe (\o -> o .: "sessionAuthKey") json1 :: Maybe String
            request methodGet "/verifyAuth" [("auth-key", pack sauth)] "" `shouldRespondWith` 401
        it "401s action if no auth" $ do
            -- steve@kolls.net 1234
            r1 <- request methodPost  "/login" [(hAuthorization, "Basic c3RldmVAa29sbHMubmV0OjEyMzQ=")] ""
            request methodGet "/verifyAuth" [] "" `shouldRespondWith` 401

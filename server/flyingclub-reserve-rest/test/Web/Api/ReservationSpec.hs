{-# LANGUAGE OverloadedStrings #-}
module Web.Api.ReservationSpec where

import           Control.Monad                            (when)
import           Control.Monad.Logger
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString                          (append)
import           Data.ByteString.Char8                    (pack)
import           Data.ByteString.Lazy                     (fromStrict)
import           Data.Maybe
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime.TimeZone.Olson
import           Data.Time.LocalTime.TimeZone.Series
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
import qualified Database.Persist.Schema                  as S
import           Web.Application


sampleUser1 = User "test1f" "test1l" Officer Nothing
sampleUser2 = User "test2f" "test2l" Social Nothing

prepAuth = do
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

prepDb = do
    (i1, i2) <- prepAuth
    i3 <- liftIO $ S.UserKey <$> (randomIO :: IO UUID)
    ia1 <- liftIO $ S.AirplaneKey <$> (randomIO :: IO UUID)
    ia2 <- liftIO $ S.AirplaneKey <$> (randomIO :: IO UUID)
    ia3 <- liftIO $ S.AirplaneKey <$> (randomIO :: IO UUID)
    insertKey i3 sampleNAUser
    insertKey ia1 a1
    insertKey ia2 a2
    insertKey ia3 a3
    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    let rkr = S.Reservation i2 ia1 (UTCTime (fromGregorian 2027 01 20) (8*60*60)) (UTCTime (fromGregorian 2027 01 20) (10*60*60)) Nothing False ""
    insertKey rk rkr
    let rk1 = rk

    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i1 ia1
      (UTCTime (fromGregorian 2007 01 20) (12*60*60))
      (UTCTime (fromGregorian 2007 01 20) (15*60*60))
      Nothing False ""

    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i1 ia1
      (UTCTime (fromGregorian 2027 01 20) (12*60*60))
      (UTCTime (fromGregorian 2027 01 20) (15*60*60))
      Nothing False ""
    let ork = rk

    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i2 ia1
      (UTCTime (fromGregorian 2027 01 20) (15*60*60))
      (UTCTime (fromGregorian 2027 01 20) (16*60*60))
      Nothing False ""

    -- for day testing, let's make one on utc day but not calendar day!
    rk <- liftIO $ S.ReservationKey <$> (randomIO :: IO UUID)
    insertKey rk $ S.Reservation
      i2 ia1
      (UTCTime (fromGregorian 2027 01 20) (1*60*60))
      (UTCTime (fromGregorian 2027 01 20) (2*60*60))
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


sampleOfficerUser = S.User "test1f" "test1l" Officer Nothing
samplePilotUser = S.User "test1f" "test1l" Pilot Nothing
sampleSocialUser = S.User "test1f" "test1l" Social Nothing
sampleNAUser = S.User "test1f" "test1l" NoAccess Nothing

a1 = S.Airplane "54073" "cessna 172" Nothing
a2 = S.Airplane "52349" "cessna 182" Nothing
a3 = S.Airplane "666ab" "cessna 666" (Just $ UTCTime (fromGregorian 2010 01 10) 0)



app :: IO Application
app = do
  tzs <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/America/Los_Angeles"
  db <- runInDb
  (DBE.sql db) prepDb
  return $ application $ ReserveRoute (DBE.sql db) id tzs

spec :: Spec
spec = with app $ do
    describe "GET /reservation/user" $ do
        it "returns future reservations for the user" $ do
            -- steve@kolls.net 1234
            r1 <- request methodPost  "/login" [(hAuthorization, "Basic c3RldmVAa29sbHMubmV0OjEyMzQ=")] ""
            let (Just json1) = decode (simpleBody r1) :: Maybe Object
            let (Just sid) = parseMaybe (\o -> o .: "id") json1 :: Maybe String
            let (Just userid1) = parseMaybe (\o -> o .: "sessionUserId") json1 :: Maybe String
            let (Just sauthkey) = parseMaybe (\o -> o .: "sessionAuthKey") json1 :: Maybe String

            r2 <- request methodGet (append "/reservation/user/" (pack userid1)) [(hAuthorization, append "Bearer " (pack sid)), ("auth-key", (pack sauthkey))] ""
            let (Just result) = decode (simpleBody r2) :: Maybe Array
            liftIO $ length result `shouldBe` 2 -- two future, non-deleted

    describe "GET /reservation/day" $ do
        it "returns all reservations for the day" $ do
            -- steve@kolls.net 1234
            r1 <- request methodPost  "/login" [(hAuthorization, "Basic c3RldmVAa29sbHMubmV0OjEyMzQ=")] ""
            let (Just json1) = decode (simpleBody r1) :: Maybe Object
            let (Just sid) = parseMaybe (\o -> o .: "id") json1 :: Maybe String
            let (Just userid1) = parseMaybe (\o -> o .: "sessionUserId") json1 :: Maybe String
            let (Just sauthkey) = parseMaybe (\o -> o .: "sessionAuthKey") json1 :: Maybe String

            r2 <- request methodGet "/reservation/day/2027-01-20" [(hAuthorization, append "Bearer " (pack sid)), ("auth-key", (pack sauthkey))] ""
            let (Just result) = decode (simpleBody r2) :: Maybe Array
            liftIO $ length result `shouldBe` 4 -- four on day, not including 1 deleted

            r2 <- request methodGet "/reservation/day/2007-01-20" [(hAuthorization, append "Bearer " (pack sid)), ("auth-key", (pack sauthkey))] ""
            let (Just result) = decode (simpleBody r2) :: Maybe Array
            liftIO $ length result `shouldBe` 1

    describe "POST /reservation" $ do
        it "creates a reservation" $ do
            -- steve@kolls.net 1234
            r1 <- request methodPost  "/login" [(hAuthorization, "Basic c3RldmVAa29sbHMubmV0OjEyMzQ=")] ""
            let (Just json1) = decode (simpleBody r1) :: Maybe Object
            let (Just sid) = parseMaybe (\o -> o .: "id") json1 :: Maybe String
            let (Just userid1) = parseMaybe (\o -> o .: "sessionUserId") json1 :: Maybe String
            let (Just sauthkey) = parseMaybe (\o -> o .: "sessionAuthKey") json1 :: Maybe String

            ra1 <- request methodGet "/airplanes"  [(hAuthorization, append "Bearer " (pack sid)), ("auth-key", (pack sauthkey))] ""
            let (Just resulta1) = decode (simpleBody ra1) :: Maybe [Object]
            let (Just airplaneid1) = parseMaybe (\o -> o .: "id") (head resulta1) :: Maybe String

            let postbody = "{\"reservationUserId\": \"" `append` (pack userid1) `append` "\", \"reservationAirplaneId\": \"" `append` (pack airplaneid1) `append`
                            "\", \"reservationStart\": \"2025-01-20T10:00:00\", \"reservationEnd\": \"2025-01-20T12:00:00\", \"reservationMaintenance\": false, \"reservationComment\": \"\"}"
            (request methodPost "/reservation" [(hAuthorization, append "Bearer " (pack sid)), ("auth-key", (pack sauthkey))] $ fromStrict postbody)
              `shouldRespondWith` 200

            r2 <- request methodGet "/reservation/day/2025-01-20" [(hAuthorization, append "Bearer " (pack sid)), ("auth-key", (pack sauthkey))] ""
            let (Just result) = decode (simpleBody r2) :: Maybe Array
            liftIO $ length result `shouldBe` 1 -- just created!

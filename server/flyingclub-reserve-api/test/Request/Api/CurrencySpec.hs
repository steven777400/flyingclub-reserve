{-# LANGUAGE OverloadedStrings #-}
module Request.Api.CurrencySpec where

import           Control.Exception.Unauthorized
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Time.Calendar
import           Data.Time.Clock
import qualified Database.Persist.Audit.Operations   as A
import           Database.Persist.Environment.Sqlite (runInMemory)
import qualified Database.Persist.Schema             as S
import           Database.Persist.Sqlite
import           Database.Persist.Types.UserType
import           Database.Persist.Types.UUID
import           Request.Api.AuthorizedAction
import           System.Random

import           Request.Api.Currency
import           Test.Hspec

anyUnauthorizedException :: Selector UnauthorizedException
anyUnauthorizedException = const True

data SampleData = SampleData {
  officerUser :: Key S.User,
  pilotUser   :: Key S.User,
  naUser      :: Key S.User,
  currency1a  :: Key S.Currency,
  currency1b  :: Key S.Currency,
  currency2   :: Key S.Currency
}

sampleOfficerUser = S.User "test1f" "test1l" Officer (fromGregorian 1990 1 1) Nothing
samplePilotUser = S.User "test1f" "test1l" Pilot (fromGregorian 1990 1 1) Nothing
sampleSocialUser = S.User "test1f" "test1l" Social (fromGregorian 1990 1 1) Nothing
sampleNAUser = S.User "test1f" "test1l" NoAccess (fromGregorian 1990 1 1) Nothing


runInDb :: (SampleData -> S.SqlM a) -> IO a
runInDb sql = runInMemory $ do
    S.runAdjustedMigration
    i1 <- liftIO $ S.UserKey <$> (randomIO :: IO UUID)
    i2 <- liftIO $ S.UserKey <$> (randomIO :: IO UUID)
    i3 <- liftIO $ S.UserKey <$> (randomIO :: IO UUID)    
    insertKey i1 sampleOfficerUser
    insertKey i2 samplePilotUser
    insertKey i3 sampleNAUser

    c1a <- liftIO $ S.CurrencyKey <$> (randomIO :: IO UUID)
    let c1ar = S.Currency i1 "name1a" Nothing Nothing Nothing (fromGregorian 1990 1 2) "comment1a" Nothing
    insertKey c1a c1ar

    c1b <- liftIO $ S.CurrencyKey <$> (randomIO :: IO UUID)
    let c1br = S.Currency i1 "name1b" (Just $ fromGregorian 2017 4 20) (Just $ fromGregorian 2015 7 8) Nothing (fromGregorian 1994 5 8) "comment1b" Nothing
    insertKey c1b c1br

    c2 <- liftIO $ S.CurrencyKey <$> (randomIO :: IO UUID)
    let c2r = S.Currency i2 "name2" (Just $ fromGregorian 2017 4 20) (Just $ fromGregorian 2015 7 8) (Just 1) (fromGregorian 1994 5 8) "comment2" Nothing
    insertKey c2 c2r

    sql (SampleData i1 i2 i3 c1a c1b c2)


spec :: Spec
spec = do
    describe "getCurrencies" $ do
        it "returns currencies" $ runInDb $ \sd -> do
          r <- runAuthorizedAction (officerUser sd) (
            getCurrencies )
          liftIO $ length r `shouldBe` 3


        it "throws for non-officer users" $ (runInDb $  \sd ->
            
            runAuthorizedAction (pilotUser sd) getCurrencies
            ) `shouldThrow` anyUnauthorizedException
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.ReserveRoute
import           Data.Time.Calendar
import           Data.Time.LocalTime.TimeZone.Olson
import qualified Database.Persist.Audit.Operations        as A
import qualified Database.Persist.Environment.Environment as DBE
import           Database.Persist.Environment.Sqlite
import qualified Database.Persist.MySQL          as MP
import           Database.Persist.Schema
import           Database.Persist.Sql
import           Database.Persist.Types.PIN
import           Database.Persist.Types.UserType
import           Database.Persist.Types.UUID
import           Network.Wai
--import qualified Network.Wai.Handler.FastCGI    as FCGI (run)
import qualified Network.Wai.Handler.Warp        as Warp (run)
--import           Network.Wai.Middleware.Approot

import           Network.Wai.Middleware.RequestLogger
import           System.Random
import           Web.Application
import           Web.Route

devConnStr = MP.defaultConnectInfo {
  MP.connectUser = "test",
  MP.connectPassword = "test",
  MP.connectDatabase = "test"}


devUser :: IO ()
devUser = runStderrLoggingT $ MP.withMySQLPool devConnStr 10 $ \pool -> liftIO $ do
    flip MP.runSqlPersistMPool pool $ do
        MP.getMigration migrateAll -- just to force type
        u1 <- liftIO $ UserKey <$> (randomIO :: IO UUID)
        MP.insertKey u1 $ User "Steve" "K" Officer (fromGregorian 2018 01 01) Nothing
        e1 <- liftIO $ EmailKey <$> (randomIO :: IO UUID)
        MP.insertKey e1 $ Email u1 "" "steve@kolls.net" True True Nothing



showMigration :: IO ()
showMigration = runStderrLoggingT $ MP.withMySQLPool devConnStr 10 $ \pool -> liftIO $ do
    flip MP.runSqlPersistMPool pool $ do
      MP.printMigration migrateAll


insertDevData :: SqlM ()
insertDevData = do
  i1 <- liftIO $ UserKey <$> (randomIO :: IO UUID)
  insertKey i1 $ User "Steve" "Kollmansberger" Officer (fromGregorian 2018 01 01) Nothing
  A.insert i1 $ Email i1 "home" "steve@kolls.net" True True Nothing
  a1 <- liftIO $ AuthenticationKey <$> (randomIO :: IO UUID)
  insertKey a1 $ Authentication i1 0 (toPIN "1234")
  A.insert i1 $ Airplane "54073" "Cessna 172" Nothing
  logDebugN "Done inserting dev data"

dev :: IO ()
dev = do
  tzs <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/America/Los_Angeles"
  db <- runInDb
  (DBE.sql db) insertDevData
  Warp.run 8080 $ application $ ReserveRoute (DBE.sql db) logStdoutDev tzs

main = dev
      

{--
dev :: IO ()
dev = do
    migrate devConnStr
    print "http://localhost:8080/"
    prepareApp devConnStr $ Warp.run 8080
--}

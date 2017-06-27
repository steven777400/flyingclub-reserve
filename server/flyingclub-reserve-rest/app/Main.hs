{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Logger
import           Control.Monad.Trans
import           Data.ReserveRoute
import           Data.Time.LocalTime.TimeZone.Olson
import qualified Database.Persist.Audit.Operations        as A
import qualified Database.Persist.Environment.Environment as DBE
import           Database.Persist.Environment.Sqlite
import           Database.Persist.Schema
import           Database.Persist.Sql
import           Database.Persist.Types.PIN
import           Database.Persist.Types.UserType
import           Database.Persist.Types.UUID
import           Network.Wai
import qualified Network.Wai.Handler.Warp                 as Warp (run)
import           Network.Wai.Middleware.RequestLogger
import           System.Random
import           Web.Application
import           Web.Route

insertDevData :: SqlM ()
insertDevData = do
  i1 <- liftIO $ UserKey <$> (randomIO :: IO UUID)
  insertKey i1 $ User "Steve" "Kollmansberger" Officer Nothing
  A.insert i1 $ Email i1 "home" "steve@kolls.net" True True Nothing
  a1 <- liftIO $ AuthenticationKey <$> (randomIO :: IO UUID)
  insertKey a1 $ Authentication i1 0 (toPIN "1234")
  logDebugN "Done inserting dev data"

dev :: IO ()
dev = do
  tzs <- getTimeZoneSeriesFromOlsonFile "/usr/share/zoneinfo/America/Los_Angeles"
  db <- runInDb
  (DBE.sql db) insertDevData
  Warp.run 8080 $ application $ ReserveRoute (DBE.sql db) logStdoutDev tzs

main = dev

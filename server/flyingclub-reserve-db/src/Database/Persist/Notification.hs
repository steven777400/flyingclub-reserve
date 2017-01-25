module Database.Persist.Notification (sendNotification, getPendingNotifications) where

import           Control.Monad.IO.Class      (liftIO)
import           Data.Time.Clock
import qualified Database.Persist.Schema     as S
import           Database.Persist.Sql        ((<.), (==.), (>.))
import qualified Database.Persist.Sql        as DB
import           Database.Persist.Types.UUID
import           System.Random


getPendingNotifications :: S.SqlM [DB.Entity S.Notification]
getPendingNotifications = DB.selectList [S.NotificationSent ==. Nothing] []

sendNotification :: DB.Key S.User -> String -> S.SqlM ()
sendNotification userId content = do
  uuid <- liftIO (randomIO :: IO UUID)
  now <- liftIO getCurrentTime
  let nt = S.Notification userId now Nothing content
  DB.insertKey (S.NotificationKey uuid) nt

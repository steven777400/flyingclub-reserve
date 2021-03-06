{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Request.Api.Reservation (
  getReservations, getReservationsDeleted, getReservationsUser,
  createReservation, updateReservation, deleteReservation) where

import           Control.Exception.Conflict
import           Control.Exception.StackError
import           Control.Monad
import           Control.Monad.IO.Class            (liftIO)
import           Data.List                         (partition)
import           Data.Time.Clock
import           Database.Persist.Audit.Operations
import           Database.Persist.Owner.Operations
import           Database.Persist.Notification
import qualified Database.Persist.Schema           as S
import           Database.Persist.Sql              ((<.), (=.), (==.), (>.), (<=.))
import qualified Database.Persist.Sql              as DB
import           Database.Persist.Types.UserType
import           Database.Persist.Types.UUID
import           Prelude                           hiding (error)
import           Request.Api.AuthorizedAction
{--
  For range, we want to allow 10am to 11am and 11am to noon
  so the idea is to include the start time and up to but not including
  the end Time

  the base formula for range overlap is
  x1 <= y2 && y1 <= x2
  let x = db and y = params
  we have to have DB values on left, so
  DBstart <= ParamEnd && DBEnd > ParamStart
  now the first <= turns into < to implement this "edge"

--}

makeReservationFilter :: UTCTime -> UTCTime -> [DB.Filter S.Reservation]
makeReservationFilter start end =
  [S.ReservationStart <=. end, S.ReservationEnd >. start]

reservationSort :: DB.SelectOpt (S.Reservation)
reservationSort = DB.Asc (S.ReservationStart)

getReservations :: UTCTime -> UTCTime -> AuthorizedAction [DB.Entity S.Reservation]
getReservations start end =
  authorize Social $ const $ DB.selectList
    (notDeleted:(makeReservationFilter start end)) [reservationSort]

getReservationsDeleted :: UTCTime -> UTCTime -> AuthorizedAction [DB.Entity S.Reservation]
getReservationsDeleted start end =
  authorize Officer $ const $ DB.selectList
    (makeReservationFilter start end) [reservationSort]

getReservationsUser :: DB.Key S.User -> AuthorizedAction [DB.Entity S.Reservation]
getReservationsUser userId =
  authorize Social $ const $ do
    now <- liftIO getCurrentTime
    DB.selectList [notDeleted, ownedBy userId, S.ReservationEnd >. now] [reservationSort]

completeResTransaction :: DB.Key S.User -> DB.Entity S.Reservation -> S.SqlM ()
completeResTransaction userId (DB.Entity resId res@S.Reservation{..}) =
  if (S.reservationMaintenance res)
  then do
    overlap <- DB.selectList
      ((S.ReservationAirplaneId ==. reservationAirplaneId):notDeleted:(makeReservationFilter reservationStart reservationEnd)) []
    let (mine, others) = partition (\r -> DB.entityKey r == resId) overlap
    when (length mine /= 1) $ DB.transactionUndo >> throw (ConflictException "Failed to insert/update reservation")
    mapM_ (\r -> delete userId (DB.entityKey r)) others
    DB.transactionSave
    mapM_ (\r -> sendNotification (S.reservationUserId $ DB.entityVal r) "TODO") others

  else do
    overlap <- DB.count
      ((S.ReservationAirplaneId ==. reservationAirplaneId):notDeleted:(makeReservationFilter reservationStart reservationEnd))
    -- should be exactly 1, the 1 we just inserted or updated
    case overlap of
      0 -> DB.transactionUndo >>
        throw (ConflictException "Failed to insert/update reservation")
      1 -> DB.transactionSave
      _ -> DB.transactionUndo >>
        throw (ConflictException "Another reservation already exists for that airplane at that time")


createReservation :: S.Reservation -> AuthorizedAction (DB.Key S.Reservation)
createReservation res@S.Reservation{..} =
  (authorize Officer cr) <> -- officer creates for anyone
  (authorizeUser reservationUserId Pilot cr) -- pilot creates for themselves
  where cr user = let
          userId = DB.entityKey user
          in do
          -- reservation must be in the future, and end after start
          now <- liftIO getCurrentTime
          when (reservationStart < now || reservationEnd < now || reservationEnd <= reservationStart) $
            throw (ConflictException "Time must be in the future and end after start")
          -- airplane must not be deleted
          getNotDeletedOrNotFound reservationAirplaneId
          -- user must not be deleted
          getNotDeletedOrNotFound reservationUserId
          -- if maint type, user must be Officer
          when (reservationMaintenance && S.userPermission (DB.entityVal user) < Officer) $
            throw (ConflictException "Only officer can create maintenance reservation")

          -- start a new transaction
          DB.transactionSave
          key <- insert userId res
          completeResTransaction userId (DB.Entity key res)
          when (userId /= reservationUserId) $ sendNotification reservationUserId "TODO"
          return key



-- we only allow update to change start and end time
updateReservation :: DB.Key S.Reservation -> UTCTime -> UTCTime -> AuthorizedAction ()
updateReservation resId start end =
  authorizeOwnerOfficer resId ur
  where    
    ur user = (DB.entityVal <$> getNotDeletedOrNotFound resId) >>= updateRes user
    updateRes user res@S.Reservation{..} = do
      now <- liftIO getCurrentTime
      when (end < now || end <= start) $
        throw (ConflictException "End time must be in future and after start")
      when (start < now && start /= reservationStart) $
        throw (ConflictException "Start time in past cannot be changed")
      -- start a new transaction
      let userId = DB.entityKey user
      DB.transactionSave
      res' <- update userId resId [S.ReservationStart =. start, S.ReservationEnd =. end]
      completeResTransaction userId (DB.Entity resId res')
      when (userId /= reservationUserId) $ sendNotification reservationUserId "TODO"

deleteReservation :: DB.Key S.Reservation -> AuthorizedAction Bool -- True if fully deleted, false is truncated
deleteReservation resId =
  (authorize Officer ur) <> -- officer deletes for anyone
  (authorizeUserM auth Pilot ur) -- pilot deletes for themselves. We have to look up who that is, though!
  where
    auth = getNotDeletedOrNotFound resId >>= return.(S.reservationUserId).(DB.entityVal)
    ur user = (DB.entityVal <$> getNotDeletedOrNotFound resId) >>= updateRes user
    updateRes user res@S.Reservation{..} = do
      now <- liftIO getCurrentTime
      let userId = DB.entityKey user
      DB.transactionSave
      when (reservationEnd < now) $
        throw (ConflictException "Can't delete reservation that is already ended")
      result <- if reservationStart < now then
          update userId resId [S.ReservationEnd =. now] >> return False
      else
          delete userId resId >> return True
      when (userId /= reservationUserId) $ sendNotification reservationUserId "TODO"
      return result

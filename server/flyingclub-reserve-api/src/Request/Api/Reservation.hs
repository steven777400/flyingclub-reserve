module Request.Api.Reservation (
  getReservations, getReservationsDeleted, getReservationsUser,
  createReservation) where

import           Control.Exception.Conflict
import           Control.Exception.StackError
import           Control.Monad.IO.Class            (liftIO)
import           Data.Time.Clock
import           Database.Persist.Audit.Operations
import           Database.Persist.Notification
import qualified Database.Persist.Schema           as S
import           Database.Persist.Sql              ((<.), (==.), (>.))
import qualified Database.Persist.Sql              as DB
import           Database.Persist.Types.UserType
import           Database.Persist.Types.UUID
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
  [S.ReservationStart <. end, S.ReservationEnd >. start]

getReservations :: UTCTime -> UTCTime -> AuthorizedAction [DB.Entity S.Reservation]
getReservations start end =
  authorize Social $ const $ DB.selectList
    (notDeleted:(makeReservationFilter start end)) []

getReservationsDeleted :: UTCTime -> UTCTime -> AuthorizedAction [DB.Entity S.Reservation]
getReservationsDeleted start end =
  authorize Officer $ const $ DB.selectList
    (makeReservationFilter start end) []

getReservationsUser :: DB.Key S.User -> AuthorizedAction [DB.Entity S.Reservation]
getReservationsUser userId =
  authorize Social $ const $ do
    now <- liftIO getCurrentTime
    DB.selectList [notDeleted, S.ReservationEnd >. now] []


createReservation :: S.Reservation -> AuthorizedAction (DB.Key S.Reservation)
createReservation res =
  (authorize Officer rf) <> -- officer creates for anyone
  (authorizeUser (S.reservationUserId res) Pilot rf) -- pilot creates for themselves
  where
    rf :: DB.Entity S.User -> S.SqlM (DB.Key S.Reservation)
    rf user = do
      let airplaneId = S.reservationAirplaneId res
      let start = S.reservationStart res
      let end = S.reservationEnd res
      let userId = DB.entityKey user
      -- reservation must be in the future, and end after start
      now <- liftIO getCurrentTime
      if start < now || end < now || end <= start then throw (ConflictException "Time must be in the future and end after start") else return ()
      -- airplane must not be deleted
      getNotDeletedOrNotFound airplaneId
      -- user must not be deleted
      getNotDeletedOrNotFound userId
      -- if maint type, user must be Officer
      if S.reservationMaintenance res && S.userPermission (DB.entityVal user) < Officer
        then throw (ConflictException "Only officer can create maintenance reservation") else return ()
      -- start a new transaction
      DB.transactionSave
      key <- insert userId res
      overlap <- DB.count
        ((S.ReservationAirplaneId ==. airplaneId):notDeleted:(makeReservationFilter start end))
      -- should be exactly 1, the 1 we just inserted
      if overlap /= 1
        then
          DB.transactionUndo >>
          throw (ConflictException "Another reservation already exists for that airplane at that time")
        else do
          DB.transactionSave
          let resUserId = S.reservationUserId res
          if userId /= resUserId then sendNotification resUserId "TODO" else return ()
          return key

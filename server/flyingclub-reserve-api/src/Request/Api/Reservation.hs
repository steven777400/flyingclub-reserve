module Request.Api.Reservation (
  getReservations, getReservationsDeleted, getReservationsUser,
  createReservation, updateReservation) where

import           Control.Exception.Conflict
import           Control.Exception.StackError
import           Control.Monad.IO.Class            (liftIO)
import           Data.Time.Clock
import           Database.Persist.Audit.Operations
import           Database.Persist.Notification
import qualified Database.Persist.Schema           as S
import           Database.Persist.Sql              ((<.), (==.), (>.), (=.))
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

completeResTransaction :: DB.Key S.User -> S.Reservation -> S.SqlM ()
completeResTransaction userId res =
  let
    airplaneId = S.reservationAirplaneId res
    start = S.reservationStart res
    end = S.reservationEnd res
    resUserId = S.reservationUserId res
  in do
    overlap <- DB.count
      ((S.ReservationAirplaneId ==. airplaneId):notDeleted:(makeReservationFilter start end))
    -- should be exactly 1, the 1 we just inserted or updated
    case overlap of
      0 -> DB.transactionUndo >>
        throw (ConflictException "Error insert/update reservation")
      1 -> DB.transactionSave >>
        if userId /= resUserId then sendNotification resUserId "TODO" else return ()
      _ -> DB.transactionUndo >>
        throw (ConflictException "Another reservation already exists for that airplane at that time")


createReservation :: S.Reservation -> AuthorizedAction (DB.Key S.Reservation)
createReservation res =
  (authorize Officer cr) <> -- officer creates for anyone
  (authorizeUser (S.reservationUserId res) Pilot cr) -- pilot creates for themselves
  where cr user = let
          airplaneId = S.reservationAirplaneId res
          start = S.reservationStart res
          end = S.reservationEnd res
          userId = DB.entityKey user
          resUserId = S.reservationUserId res
          in do
          -- reservation must be in the future, and end after start
          now <- liftIO getCurrentTime
          if start < now || end < now || end <= start then throw (ConflictException "Time must be in the future and end after start") else return ()
          -- airplane must not be deleted
          getNotDeletedOrNotFound airplaneId
          -- user must not be deleted
          getNotDeletedOrNotFound resUserId
          -- if maint type, user must be Officer
          if S.reservationMaintenance res && S.userPermission (DB.entityVal user) < Officer
            then throw (ConflictException "Only officer can create maintenance reservation") else return ()

          -- start a new transaction
          DB.transactionSave
          key <- insert userId res
          completeResTransaction userId res
          return key
-- TODO maintenance should wipe out other reservations in the way


-- we only allow update to change start and end time
updateReservation :: DB.Entity S.Reservation -> UTCTime -> UTCTime -> AuthorizedAction ()
updateReservation res start end =
  (authorize Officer ur) <> -- officer updates for anyone
  (authorizeUser (S.reservationUserId $ DB.entityVal res) Pilot ur) -- pilot updates for themselves
  where
    ur user = do
      now <- liftIO getCurrentTime
      if end < now || end <= start then throw (ConflictException "End time must be in future and after start") else return ()
      if start < now && start /= S.reservationStart (DB.entityVal res) then throw (ConflictException "Start time in past cannot be changed") else return ()
      -- start a new transaction
      let userId = DB.entityKey user
      DB.transactionSave
      res' <- update userId (DB.entityKey res) [S.ReservationStart =. start, S.ReservationEnd =. end]
      completeResTransaction userId res'

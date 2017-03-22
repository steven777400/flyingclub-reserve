module Request.Api.ParsedAction (runParsedAction) where

import           Control.Exception.Format
import           Control.Exception.StackError
import           Control.Monad
import           Data.ParsedAction
import           Data.ParsedActionResult
import           Data.Text
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.DayRange
import           Data.Time.LocalTime.TimeZone.Series
import           Database.Persist.Audit.Operations
import           Database.Persist.Schema
import           Database.Persist.Sql
import           Request.Api.Airplane
import           Request.Api.AuthorizedAction
import           Request.Api.Reservation

singlePlane :: Key User -> TailNumber -> (Entity Airplane -> SqlM ParsedActionResult) -> SqlM ParsedActionResult
singlePlane userId tailn f = do
  planes <- runAuthorizedAction userId (findAirplanes tailn)
  case planes of
    [p] -> f p
    [] -> throw (FormatException "Tail number not found")
    _ -> throw (FormatException "Ambigious tail number, provide additional digits")

check :: ZoneSeriesTime -> Key User -> TailNumber -> Day -> SqlM ParsedActionResult
check zst userId tailn day = do
  singlePlane userId tailn (\p -> do
      let (begin, end) = dayRange zst day
      let utcnow = zoneSeriesTimeToUTC zst
      when (end < utcnow) $ throw (FormatException "Can't check past dates")
      res <- runAuthorizedAction userId (getReservations begin end)
      -- need to filter down to
      let res' = filterE (\x->
            -- just the airplane
            reservationAirplaneId x == entityKey p &&
            -- just after now (in case checking today)
            reservationEnd x > utcnow
            ) res
      return $ CheckResult res'
      )

review :: ZoneSeriesTime -> Key User -> Day -> SqlM ParsedActionResult
review zst userId day = do
  let (begin, end) = dayRange zst day
  let utcnow = zoneSeriesTimeToUTC zst
  when (end < utcnow) $ throw (FormatException "Can't review past dates")
  res <- runAuthorizedAction userId (getReservations begin end)
  let res' = filterE (\x->
        -- just the user
        reservationUserId x == userId &&
        -- just after now (in case checking today)
        reservationEnd x > utcnow
        ) res
  return $ ReviewResult res'


reserve :: Key User -> TailNumber -> UTCTime -> UTCTime -> SqlM ParsedActionResult
reserve userId tailn begin end = do
  singlePlane userId tailn (\p -> do
      let res = Reservation userId (entityKey p) begin end Nothing False empty
      resid <- runAuthorizedAction userId (createReservation res)

      return $ ReserveResult (Entity resid res)
      )

runParsedAction :: ZoneSeriesTime -> Key User -> ParsedAction -> SqlM ParsedActionResult
runParsedAction zst userId pa = case pa of
  Check tailn day         -> check zst userId tailn day
  Review day              -> review zst userId day
  Reserve tailn begin end -> reserve userId tailn begin end

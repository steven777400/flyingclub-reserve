module Request.Api.ParsedAction where

import           Control.Exception.Format
import           Control.Exception.StackError
import           Control.Monad
import           Data.ParsedAction
import           Data.ParsedActionResult
import           Data.Text
import           Data.Time.Calendar
import           Data.Time.DayRange
import           Data.Time.LocalTime.TimeZone.Series
import           Database.Persist.Audit.Operations
import           Database.Persist.Schema
import           Database.Persist.Sql
import           Request.Api.Airplane
import           Request.Api.AuthorizedAction
import           Request.Api.Reservation

check :: ZoneSeriesTime -> Key User -> TailNumber -> Day -> SqlM ParsedActionResult
check zst userId tailn day = do
  planes <- runAuthorizedAction userId (findAirplanes tailn)
  case planes of
    [p] -> do
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
    [] -> throw (FormatException "Tail number not found")
    _ -> throw (FormatException "Ambigious tail number, provide additional digits")

runParsedAction :: ZoneSeriesTime -> Key User -> ParsedAction -> SqlM ParsedActionResult
runParsedAction zst userId pa = case pa of
  Check tailn day -> check zst userId tailn day

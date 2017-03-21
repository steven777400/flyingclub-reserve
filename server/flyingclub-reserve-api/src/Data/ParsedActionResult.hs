module Data.ParsedActionResult where

import           Database.Persist.Schema
import           Database.Persist.Sql

data ParsedActionResult =
  CheckResult [Entity Reservation] |
  ReviewResult [Entity Reservation] |
  ReserveResult (Entity Reservation)

  --Reserve TailNumber UTCTime UTCTime |
  --Cancel (Maybe TailNumber) UTCTime |
  --Update UTCTime -- extend the current reservation to the given new end time

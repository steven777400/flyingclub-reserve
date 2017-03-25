module Data.ParsedActionResult where

import           Database.Persist.Schema
import           Database.Persist.Sql

data ParsedActionResult =
  CheckResult [Entity Reservation] |
  ReviewResult [Entity Reservation] |
  ReserveResult (Entity Reservation) |
  CancelResult  (Entity Reservation) Bool -- True if fully deleted, false is truncated
  
  --Update UTCTime -- extend the current reservation to the given new end time

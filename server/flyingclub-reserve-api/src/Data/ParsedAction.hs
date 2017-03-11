module Data.ParsedAction where

import           Data.Text
import           Data.Time.Calendar
import           Data.Time.Clock

type TailNumber = Text

data ParsedAction =
  Check TailNumber Day |
  Review Day |
  Reserve TailNumber UTCTime UTCTime |
  Cancel (Maybe TailNumber) UTCTime | 
  Update UTCTime -- extend the current reservation to the given new end time
  deriving (Show, Eq)

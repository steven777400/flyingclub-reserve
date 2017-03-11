module Data.ParsedAction where

import           Data.Text
import           Data.Time.Calendar
import           Data.Time.Clock

type TailNumber = Text

data ParsedAction =
  Check TailNumber Day |
  Reserve TailNumber UTCTime UTCTime
  deriving (Show, Eq)

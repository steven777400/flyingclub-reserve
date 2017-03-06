module Data.ParsedAction where

import           Data.Text
import           Data.Time.Calendar

data ParsedAction =
  Check Text Day
  deriving (Show, Eq)

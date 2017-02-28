{-# LANGUAGE OverloadedStrings #-}
module Request.Parser.Reservation where

import           Data.Attoparsec.Text
import           Data.Char
import           Data.ParsedAction
import           Data.Text
import           Data.Time.Calendar
import           Request.Parser.Day

check :: Parser (Day -> ParsedAction)
check = do
  asciiCI "check"
  skipSpace
  aid <- takeWhile1 isAlphaNum
  skipSpace
  sday <- option id dayFromToday
  return $ \d -> Check aid (sday d)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Request.Parser.Day (dayFromToday) where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Request.Parser.Utility

tomorrow :: Parser (Day -> Day)
tomorrow = pure (addDays 1) <* approxMatch "tomorrow"

today :: Parser (Day -> Day)
today = pure id <* "today"

daysToNext :: Int -> Day -> Int
daysToNext target (toWeekDate -> (_, _, weekday))
    | target > weekday  = target - weekday          -- move forward in this week
    | target == weekday = 0                         -- today!
    | otherwise         = (7 - weekday) + target    -- beginning of next week

--  day of week (1 for Monday to 7 for Sunday)
-- from https://hackage.haskell.org/package/time-1.6/docs/Data-Time-Calendar-WeekDate.html

days :: [(Int, String)]
days = [
    (1, "monday"),
    (2, "tuesday"),
    (3, "wednesday"),
    (4, "thursday"),
    (5, "friday"),
    (6, "saturday"),
    (7, "sunday")
    ]

dayOfWeek :: (Int, String) -> Parser (Day -> Day)
dayOfWeek (val, str) = pure (\d -> addDays (toInteger $ daysToNext val d) d)
    <* approxMatch str

date' :: (Int, Int) -> Day -> Day
date' (requestMonth, requestDay) (toGregorian -> (startYear, startMonth, startDay)) =
    case
        if requestMonth > startMonth || -- future month
            (requestMonth == startMonth && requestDay >= startDay) -- future day in this month
        then fromGregorianValid startYear requestMonth requestDay
        else fromGregorianValid (startYear + 1) requestMonth requestDay -- otherwise next year, to avoid the past
    of
    Just actualDay -> actualDay
    Nothing -> error "Invalid date"


datep :: Parser Int -> Parser Int -> Parser (Day -> Day)
datep monthf dayf = do
    requestMonth <- monthf
    requestDay <- dayf
    return $ date' (requestMonth, requestDay)

date :: Parser (Day -> Day)
date =
    datep (digits 2 12 <* "/") (digits 2 31) <|>
    datep (digits 1 9 <* "/") (digits 2 31) <|>
    datep (digits 2 12 <* "/") (digits 1 9) <|>
    datep (digits 1 9 <* "/") (digits 1 9) <|>
    datep (digits 2 12) (digits 2 31)

dayFromToday :: Parser (Day -> Day)
dayFromToday = today <|> tomorrow <|> choice (map dayOfWeek days) <|> date

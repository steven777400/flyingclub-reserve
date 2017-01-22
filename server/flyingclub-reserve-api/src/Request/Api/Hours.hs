{-# LANGUAGE DeriveGeneric   #-}
module Request.Api.Hours where

import Data.Aeson
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.LocalTime.TimeZone.Series
import qualified GHC.Generics                    as G
import qualified Data.Time.Horizon as H

localDayTimeRange :: TimeZoneSeries -> Day -> (UTCTime, UTCTime)
localDayTimeRange tzs day = let      
    begin = LocalTime day midnight 
    end = LocalTime day (TimeOfDay 23 59 59) 
    in
    (localTimeToUTC' tzs begin, localTimeToUTC' tzs end)

data LocalHour = LocalHour {
    hour :: Int,
    pm :: Bool,
    utc :: UTCTime
    }   
    deriving (Show, G.Generic)

instance ToJSON LocalHour where

utcToLocalHour :: TimeZoneSeries -> UTCTime -> LocalHour
utcToLocalHour tzs utc =  let 
    (LocalTime _ (TimeOfDay hr _ _)) = utcToLocalTime' tzs utc
    (friendlyHour, ispm) = if hr == 0 
            then (12, False) 
            else if hr < 12 
            then (hr, False) 
            else if hr == 12
            then (12, True)
            else (hr - 12,True)
    in
    LocalHour friendlyHour ispm utc

localDayHours :: TimeZoneSeries -> Day -> [LocalHour]
localDayHours tzs day = let      
    begin = LocalTime day midnight 
    beginUtc = localTimeToUTC' tzs begin    
    in
    takeWhile (\(LocalHour _ _ utc) -> localDay (utcToLocalTime' tzs utc) == day) 
    [utcToLocalHour tzs hrutc | cnthr <- [0..], 
        let hrutc = addUTCTime (fromIntegral $ 60*60*cnthr) beginUtc]

data LocalSunriseSunset = LocalSunriseSunset {
    sunrise :: LocalHour,
    sunset :: LocalHour
    }   
    deriving (G.Generic)

instance ToJSON LocalSunriseSunset where
        
localSunriseSunset :: (H.LongitudeWest, H.LatitudeNorth) -> TimeZoneSeries -> Day -> LocalSunriseSunset
localSunriseSunset (lon, lat) tzs day = let
    sunriseUtc = H.sunrise day lon lat
    sunsetUtc = H.sunset (addDays 1 day) lon lat -- based on utc, we want the "next utc day" sunset since utc rolls during the day in America
    in
    LocalSunriseSunset
        (utcToLocalHour tzs sunriseUtc)
        (utcToLocalHour tzs sunsetUtc)


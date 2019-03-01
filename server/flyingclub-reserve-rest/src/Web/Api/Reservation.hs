{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
module Web.Api.Reservation (getReservationsR, getReservationsDR, postReservationR) where

import           Control.Exception.Format
import           Control.Exception.StackError
import           Data.Aeson
import           Data.ReserveRoute
import qualified Data.Text                           as T
import           Data.Time.Calendar
import           Data.Time.DayRange
import           Data.Time.LocalTime
import           Data.Time.LocalTime.TimeZone.Series
import           Database.Persist.Schema
import           GHC.Generics
import           Request.Api.AuthorizedAction
import qualified Request.Api.Reservation             as R
import           Wai.Routes
import           Web.Authentication

data InputReservation = InputReservation {
  reservationUserId      :: Key User,
  reservationAirplaneId  :: Key Airplane,
  reservationStart       :: LocalTime,
  reservationEnd         :: LocalTime,
  reservationMaintenance :: Bool,
  reservationComment     :: T.Text
} deriving (Generic, FromJSON)


getReservationsR :: Key User -> Handler ReserveRoute
getReservationsR uid = authorizedRoute (R.getReservationsUser uid)

getReservationsDR :: Day -> Handler ReserveRoute
getReservationsDR day = authorizedSession $ \rr _ session -> let
  userId = sessionUserId session
  zst = timeZoneSeries rr
  (begin, end) = dayRange zst day
  in
  runAuthorizedAction userId (R.getReservations begin end)


inputReservation :: TimeZoneSeries -> InputReservation -> Reservation
inputReservation tzs ir@InputReservation{..} = Reservation
  reservationUserId
  reservationAirplaneId
  (localTimeToUTC' tzs reservationStart)
  (localTimeToUTC' tzs reservationEnd)  
  reservationMaintenance
  reservationComment
  Nothing

postReservationR :: Handler ReserveRoute
postReservationR = authorizedSession $ \rr body session -> do
  let userId = sessionUserId session
  let ires = (decodeStrict body) :: Maybe InputReservation
  case ires of
    Just ires' -> do
      let res = inputReservation (timeZoneSeries rr) ires'
      runAuthorizedAction userId (R.createReservation res)
    Nothing -> throw $ FormatException "Invalid reservation input format"




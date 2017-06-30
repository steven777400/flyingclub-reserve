{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Web.Api.Reservation (postReservationR) where

import           Control.Exception.Format
import           Control.Exception.StackError
import           Data.Aeson
import           Data.ReserveRoute
import qualified Data.Text                           as T
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

inputReservation :: TimeZoneSeries -> InputReservation -> Reservation
inputReservation tzs ir@InputReservation{..} = Reservation
  reservationUserId
  reservationAirplaneId
  (localTimeToUTC' tzs reservationStart)
  (localTimeToUTC' tzs reservationEnd)
  Nothing
  reservationMaintenance
  reservationComment

postReservationR :: Handler ReserveRoute
postReservationR = authorizedSession $ \rr body session -> do
  let userId = sessionUserId session
  let ires = (decodeStrict body) :: Maybe InputReservation
  case ires of
    Just ires' -> do
      let res = inputReservation (timeZoneSeries rr) ires'
      result <- runAuthorizedAction userId (R.createReservation res)
      return result
    Nothing -> throw $ FormatException "Invalid reservation input format"




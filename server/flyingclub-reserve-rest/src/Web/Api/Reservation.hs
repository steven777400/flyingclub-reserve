{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Web.Api.Reservation (postReservationR) where

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
  userId      :: Key User,
  airplaneId  :: Key Airplane,
  localStart  :: LocalTime,
  localEnd    :: LocalTime,
  maintenance :: Bool,
  comment     :: T.Text
} deriving (Generic, FromJSON)

inputReservation :: TimeZoneSeries -> InputReservation -> Reservation
inputReservation tzs ir@InputReservation{..} = Reservation
  userId
  airplaneId
  (localTimeToUTC' tzs localStart)
  (localTimeToUTC' tzs localEnd)
  Nothing
  maintenance
  comment

postReservationR :: Handler ReserveRoute
postReservationR = authorizedSession $ \rr body session -> do
  let userId = sessionUserId session
  let ires = (decodeStrict body) :: Maybe InputReservation
  case ires of
    Just ires' -> do
      let res = inputReservation (timeZoneSeries rr) ires'
      result <- runAuthorizedAction userId (R.createReservation res)
      return $ Right result
    Nothing -> return $ Left status400 -- TODO let's replace this with a throw and have the error handler middleware handle it




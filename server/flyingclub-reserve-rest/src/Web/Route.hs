{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
module Web.Route where

import           Data.ReserveRoute
import           Data.Time.Calendar
import           Database.Persist.Schema
import           Network.HTTP.Types
import           Wai.Routes
import           Web.Api.Airplane
import           Web.Api.Reservation
import           Web.Api.Session
import           Web.Api.User
import           Web.Authentication


mkRoute "ReserveRoute" [parseRoutes|
/login                      LoginR          POST
/verifyAuth                 VerifyAuthR     GET

/users                      UsersR          GET
/user/#UserId               UserR           GET

/airplanes                  AirplanesR      GET

/reservation/user/#UserId   ReservationsR   GET
/reservation/day/#Day       ReservationsDR  GET
/reservation                ReservationR    POST
|]



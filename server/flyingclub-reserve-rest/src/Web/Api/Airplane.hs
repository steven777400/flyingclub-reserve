{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
module Web.Api.Airplane (getAirplanesR) where

import           Data.ReserveRoute
import           Database.Persist.Schema
import qualified Request.Api.Airplane    as A
import           Wai.Routes
import           Web.Authentication


getAirplanesR :: Handler ReserveRoute
getAirplanesR = authorizedRoute (A.getAirplanes)




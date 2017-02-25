{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Web.Api.User (getUsersR) where

import           Data.ReserveRoute
import qualified Request.Api.User   as U
import           Wai.Routes
import           Web.Authentication

getUsersR :: Handler ReserveRoute
getUsersR = authorizedRoute (U.getUsers)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
module Web.Api.User (getUsersR, getUserR) where

import           Data.ReserveRoute
import           Database.Persist.Schema
import qualified Request.Api.User        as U
import           Wai.Routes
import           Web.Authentication


getUsersR :: Handler ReserveRoute
getUsersR = authorizedRoute (U.getUsersDetails)

getUserR :: Key User -> Handler ReserveRoute
getUserR uid = authorizedRoute (U.getUserDetails uid)

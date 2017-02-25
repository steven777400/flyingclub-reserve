{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-} -- EXTREME DANGER: Prettify removes this!
module Web.Route where

import Data.ReserveRoute
import Database.Persist.Schema
import Network.HTTP.Types
import Wai.Routes
import Web.Api.Session
import Web.Api.User
import Web.Authentication




mkRoute "ReserveRoute" [parseRoutes|
/users              UsersR GET
/user/#UserId       UserR GET
/login              LoginR POST
/verifyAuth         VerifyAuthR GET
|]


getUserR x = undefined

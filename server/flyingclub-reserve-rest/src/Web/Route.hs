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
import Web.Session




mkRoute "ReserveRoute" [parseRoutes|
/users              UsersR GET
/user/#UserId       UserR GET
/login              LoginR POST
|]

getUsersR = undefined
getUserR x = undefined

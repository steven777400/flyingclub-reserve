module Request.Api.AuthorizedAction (AuthorizedAction,
    authorize, authorizeCond, authorizeUser,
    (<>),     -- re-export from Data.Semigroup
    runAuthorizedAction) where

import           Control.Exception.StackError
import           Control.Exception.Unauthorized
import           Data.Semigroup
import           Database.Persist
import           Database.Persist.Schema
import           Database.Persist.Types.UserType
import           Prelude                         hiding (error)

newtype AuthorizedAction o = AuthorizedAction (Entity User -> Maybe (SqlM o))

authorizeCond :: (Entity User -> Bool) -> (Entity User -> SqlM o) -> AuthorizedAction o
authorizeCond authf act = AuthorizedAction $ \user ->
    if authf user
    then Just $ act user
    else Nothing

authorize :: UserType -> (Entity User -> SqlM o) -> AuthorizedAction o
authorize userType = authorizeCond (\(Entity _ user) ->
    userPermission user >= userType)

authorizeUser :: Key User -> UserType -> (Entity User -> SqlM o) -> AuthorizedAction o
authorizeUser reqUserId userType = authorizeCond (\(Entity actUserId user) ->
    reqUserId == actUserId && userPermission user >= userType)

instance Semigroup (AuthorizedAction a) where
    (<>) (AuthorizedAction a1) (AuthorizedAction a2) = AuthorizedAction $ \euser ->
        case a1 euser of
            Just r1 -> Just r1  -- If the first request succeeds, take it
            Nothing -> a2 euser -- otherwise take the second request


runAuthorizedAction :: Key User -> AuthorizedAction o -> SqlM o
runAuthorizedAction userId (AuthorizedAction auth) = do
    user <- get userId
    case user of
        Just user' -> case auth (Entity userId user') of
            Just r1 -> r1
            Nothing -> throw UnauthorizedException
        Nothing -> error "User not found"

module Request.Api.AuthorizedAction (AuthorizedAction,
    authorize, authorizeCond, authorizeUserM, authorizeUser,
    (<>),     -- re-export from Data.Semigroup
    runAuthorizedAction) where

import           Control.Exception.StackError
import           Control.Exception.Unauthorized
import           Data.Semigroup
import           Database.Persist
import           Database.Persist.Schema
import           Database.Persist.Types.UserType
import           Prelude                         hiding (error)

newtype AuthorizedAction o = AuthorizedAction (Entity User -> SqlM (Maybe o))

authorizeCond :: (Entity User -> SqlM Bool) -> (Entity User -> SqlM o) -> AuthorizedAction o
authorizeCond authf act = AuthorizedAction $ \user -> do
    auth <- authf user
    if auth
    then Just <$> act user
    else return Nothing

authorize :: UserType -> (Entity User -> SqlM o) -> AuthorizedAction o
authorize userType = authorizeCond (\(Entity _ user) ->
    return $ userPermission user >= userType)

authorizeUserM :: SqlM (Key User) -> UserType -> (Entity User -> SqlM o) -> AuthorizedAction o
authorizeUserM reqUserIdM userType = authorizeCond (\(Entity actUserId user) -> reqUserIdM >>= \reqUserId ->
    return $ reqUserId == actUserId && userPermission user >= userType)


authorizeUser :: Key User -> UserType -> (Entity User -> SqlM o) -> AuthorizedAction o
authorizeUser reqUserId = authorizeUserM (return reqUserId)

instance Semigroup (AuthorizedAction a) where
    (<>) (AuthorizedAction a1) (AuthorizedAction a2) = AuthorizedAction $ \euser -> do
        res <- a1 euser
        case res of
            Just r1 -> return $ Just r1  -- If the first request succeeds, take it
            Nothing -> a2 euser -- otherwise take the second request


runAuthorizedAction :: Key User -> AuthorizedAction o -> SqlM o
runAuthorizedAction userId (AuthorizedAction auth) = do
    user <- get userId
    case user of
        Just user' -> do
          res <- auth (Entity userId user')
          case res of
            Just r1 -> return r1
            Nothing -> throw UnauthorizedException
        Nothing -> error "User not found"
